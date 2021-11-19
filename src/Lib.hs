{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative ((<|>))
import Control.Exception.Base (throw)
import Control.Lens (_16')
import Control.Lens.Internal.Zoom (Effect (Effect))
import Control.Monad (void)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, runExceptT, throwError)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (except)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (JSONPathElement (Key), Parser, parserThrowError, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import qualified Data.ByteString.UTF8 as UTF8
import Data.Foldable (foldl')
import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Strict as Map (filterWithKey, lookup, singleton, toList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (addUTCTime, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat)
import Data.Time.ISO8601 (formatISO8601Micros)
import GHC.Generics (Generic)
import Network.HTTP.Client (Request (checkResponse, method, requestBody, requestHeaders), RequestBody (RequestBodyLBS), Response, httpLbs)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP (tlsManagerSettings)
import Network.HTTP.Types (Header, Method, methodPost)
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI, nullURI, parseURI)
import Pipes (Producer, runEffect, (>->))
import qualified Pipes.Prelude as P
import qualified Pipes.Prelude as PP
import Pipes.Safe (MonadMask, MonadSafe, runSafeT)
import qualified Pipes.Safe as Pipes (bracket)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getHomeDirectory, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((<.>), (</>))
import Systemd.Journal
import qualified Systemd.Journal as Priority
import Systemd.Journal.Upload (upToPriority)
import Text.Printf (printf)

getJournalStateFilePath :: String -> IO FilePath
getJournalStateFilePath producerId = do
  dataDir <- getXdgDirectory XdgData $ "systemd" </> "journal-upload"
  createDirectoryIfMissing True dataDir
  return $ dataDir </> ("state" <.> producerId <.> "json")

getJournalStart :: (MonadIO m) => FilePath -> ExceptT JournalUploaderError m Start
getJournalStart stateFilePath =
  ifM
    (liftIO $ doesFileExist stateFilePath)
    (readJournalUploadStateCursor stateFilePath)
    (pure (FromEnd Forwards))

makeJournalProducer :: MonadSafe m => JournalUploadConfig -> Start -> Producer JournalEntry m ()
makeJournalProducer config journalStart =
  openJournal [LocalOnly] journalStart (Just $ upToPriority $ minPriority config) Nothing (mode config) >-> PP.dropWhile discardProcessed
  where
    discardProcessed :: JournalEntry -> Bool
    discardProcessed entry = case journalStart of
      FromCursor cursor _ -> cursor == journalEntryCursor entry
      _ -> False

newtype SerializableJournalEntryCursor = SerializableJournalEntryCursor JournalEntryCursor deriving (Eq, Show)

parseBase64ByteString :: Text -> A.Parser ByteString
parseBase64ByteString =
  either (A.parserThrowError []) pure . first Text.unpack . decodeBase64 . encodeUtf8

instance FromJSON SerializableJournalEntryCursor where
  parseJSON (A.String str) = SerializableJournalEntryCursor <$> parseBase64ByteString str
  parseJSON invalid = A.prependFailure "parsing JournalEntryCursor failed, " (A.typeMismatch "String" invalid)

instance ToJSON SerializableJournalEntryCursor where
  toJSON (SerializableJournalEntryCursor cursor) = toJSON $ encodeBase64 cursor

data JournalUploadState = JournalUploadState
  { lastCursor :: SerializableJournalEntryCursor
  }
  deriving (Eq, Generic, Show)

instance FromJSON JournalUploadState

instance ToJSON JournalUploadState

readJournalUploadState :: (MonadIO m) => FilePath -> ExceptT JournalUploaderError m JournalUploadState
readJournalUploadState fp = ExceptT $ first JournalUploaderStateMalformedError <$> liftIO (A.eitherDecodeFileStrict fp)

readJournalUploadStateCursor :: (MonadIO m) => FilePath -> ExceptT JournalUploaderError m Start
readJournalUploadStateCursor fp = do
  JournalUploadState {lastCursor = SerializableJournalEntryCursor cursor} <- readJournalUploadState fp
  return $ FromCursor cursor Forwards

writeJournalUploadState :: FilePath -> JournalUploadState -> IO ()
writeJournalUploadState = A.encodeFile

data JournalUploadConfig = JournalUploadConfig
  { minPriority :: Priority,
    mode :: Mode,
    destinationUri :: URI,
    journalRequestHeaders :: [Header],
    journalRequestMethod :: Method,
    journalFields :: [(Text, Text)]
  }

defaultJournalUploadConfig :: JournalUploadConfig
defaultJournalUploadConfig =
  JournalUploadConfig
    { minPriority = Priority.Warning,
      mode = Waiting,
      destinationUri = nullURI,
      journalRequestHeaders = mempty,
      journalRequestMethod = methodPost,
      journalFields = [("_UID", "uid"), ("MESSAGE", "message")]
    }

papertrailUploadConfig token =
  let maybeUri = parseURI "https://logs.collector.solarwinds.com/v1/log"
   in fmap
        ( \uri ->
            defaultJournalUploadConfig
              { destinationUri = uri,
                journalRequestHeaders = [(HTTP.hAuthorization, "Basic " <> token)]
              }
        )
        maybeUri

pickJournalFields :: [(Text, Text)] -> JournalEntry -> HashMap Text Text
pickJournalFields fields journalEntry =
  foldMap (\(key, newKey) -> maybe mempty (Map.singleton newKey . decodeUtf8) $ Map.lookup (mkJournalField key) $ journalEntryFields journalEntry) fields

uploadJson :: (MonadIO m, ToJSON a) => HTTP.Manager -> HTTP.Request -> a -> m ()
uploadJson httpManager baseRequest entry = do
  let request = baseRequest {requestBody = RequestBodyLBS $ A.encode entry}
  void $ liftIO $ HTTP.httpNoBody request httpManager

syslogStructuredData :: [(Text, Text)] -> JournalEntry -> Text
syslogStructuredData fields entry = Text.intercalate " " $ fmap printPair $ Map.toList $ pickJournalFields fields entry
  where
    printPair :: (Text, Text) -> Text
    printPair (k, v) = k <> "=\"" <> v <> "\""

encodeJournalFields :: JournalUploadConfig -> JournalEntry -> BL.ByteString
encodeJournalFields config entry =
  let t = formatJournalEntryTime entry
      fields = journalEntryFields entry
      p = decodeUtf8 $ fromMaybe "6" $ Map.lookup "PRIORITY" fields
      h = decodeUtf8 $ fromMaybe "-" $ Map.lookup "_HOSTNAME" fields
      u = decodeUtf8 $ fromMaybe "-" (Map.lookup "UNIT" fields <|> Map.lookup "_SYSTEMD_UNIT" fields <|> Map.lookup "SYSLOG_IDENTIFIER" fields)
      pid = decodeUtf8 $ fromMaybe "-" $ Map.lookup "_PID" fields
      m = syslogStructuredData (journalFields config) entry
   in UTF8L.fromString $ printf "<22>%s %s %s %s %s - - [%s]" p t h u pid m

formatJournalEntryTime :: JournalEntry -> String
formatJournalEntryTime entry =
  let (seconds, micros) = toInteger (journalEntryRealtime entry) `divMod` (1000 * 1000)
      timestampUTCTime = addUTCTime (fromInteger micros / (1000 * 1000)) (posixSecondsToUTCTime (fromInteger seconds))
   in formatISO8601Micros timestampUTCTime

processJournalEntry :: (MonadIO m) => HTTP.Manager -> FilePath -> JournalUploadConfig -> Request -> JournalEntry -> m ()
processJournalEntry httpManager stateFilePath config baseRequest journalEntry = do
  let body = encodeJournalFields config journalEntry
  -- liftIO $ print body
  void $ liftIO $ HTTP.httpNoBody (baseRequest {requestBody = RequestBodyLBS body}) httpManager
  liftIO $ writeJournalUploadState stateFilePath JournalUploadState {lastCursor = SerializableJournalEntryCursor $ journalEntryCursor journalEntry}

makeJournalUploader :: MonadIO m => FilePath -> JournalUploadConfig -> IO (JournalEntry -> m ())
makeJournalUploader stateFilePath config = do
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  requestWithURI <- HTTP.requestFromURI (destinationUri config)
  let baseRequest =
        requestWithURI
          { method = journalRequestMethod config,
            requestHeaders = journalRequestHeaders config,
            checkResponse = HTTP.throwErrorStatusCodes
          }
  return $ processJournalEntry httpManager stateFilePath config baseRequest

data JournalUploaderError
  = JournalUploaderStateMalformedError String
  | JournalUploaderDestinationURIInvalid
  | JournalUploaderTokenUnavailable
  deriving (Show)

runWithConfig :: (MonadIO m, MonadMask m) => JournalUploadConfig -> ExceptT JournalUploaderError m ()
runWithConfig config = do
  stateFilePath <- liftIO $ getJournalStateFilePath "default"
  journalUploader <- liftIO $ makeJournalUploader stateFilePath config
  start <- getJournalStart stateFilePath
  let journalProducer = makeJournalProducer config start
  runSafeT $ runEffect $ journalProducer >-> P.mapM_ journalUploader

runPapertrail :: (MonadIO m, MonadMask m) => ExceptT JournalUploaderError m ()
runPapertrail = do
  maybeToken <- liftIO $ lookupEnv "PAPERTRAIL_TOKEN"
  token <- maybe (except $ Left JournalUploaderTokenUnavailable) (pure . UTF8.fromString) maybeToken
  config <- maybe (except $ Left JournalUploaderDestinationURIInvalid) pure $ papertrailUploadConfig token
  runWithConfig config

run :: IO ()
run = runExceptT runPapertrail >>= either (error . show) pure

someFunc :: IO ()
someFunc = run
