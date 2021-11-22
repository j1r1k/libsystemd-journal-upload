{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Logger.Simple (logInfo)
import Control.Monad (void)
import Control.Monad.Catch (Handler (Handler))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (except)
import Control.Retry (RetryPolicyM, RetryStatus, capDelay, exponentialBackoff, recovering)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser, parserThrowError, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8L (fromString)
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import Data.Functor (($>))
import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Strict as Map (lookup, singleton, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.ISO8601 (formatISO8601Micros)
import GHC.Generics (Generic)
import Network.HTTP.Client (Request (checkResponse, method, requestBody, requestHeaders), RequestBody (RequestBodyLBS))
import qualified Network.HTTP.Client as HTTP (HttpException (HttpExceptionRequest), HttpExceptionContent (ConnectionFailure, ConnectionTimeout, ResponseTimeout), Manager, httpNoBody, newManager, requestFromURI, throwErrorStatusCodes)
import qualified Network.HTTP.Client.TLS as HTTP (tlsManagerSettings)
import Network.HTTP.Types (Header, Method, methodPost)
import qualified Network.HTTP.Types as HTTP (hAuthorization)
import Network.URI (URI, nullURI, parseURI)
import Pipes (Producer, runEffect, (>->))
import qualified Pipes.Prelude as P (dropWhile, mapM_)
import Pipes.Safe (MonadMask, MonadSafe, runSafeT)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((<.>), (</>))
import Systemd.Journal
  ( Direction (Forwards),
    JournalEntry (..),
    JournalEntryCursor,
    JournalFlag (LocalOnly),
    Mode (Waiting),
    Priority,
    Start (FromCursor, FromEnd),
    mkJournalField,
    openJournal,
  )
import qualified Systemd.Journal as Priority (Priority (Warning))
import Systemd.Journal.Filter (upToPriority)
import Text.Printf (printf)

getJournalStateFilePath :: String -> IO FilePath
getJournalStateFilePath producerId = do
  dataDir <- getXdgDirectory XdgData $ "systemd" </> "journal-upload"
  createDirectoryIfMissing True dataDir
  return $ dataDir </> ("state" <.> producerId <.> "json")

getJournalStart :: (MonadIO m) => FilePath -> ExceptT AppError m Start
getJournalStart stateFilePath =
  ifM
    (liftIO $ doesFileExist stateFilePath)
    (readJournalUploadStateCursor stateFilePath >>= \cursor -> liftIO (logInfo "Starting from cursor") $> cursor)
    (logInfo "Starting from end of journal" $> FromEnd Forwards)

makeJournalProducer :: MonadSafe m => JournalUploadConfig -> Start -> Producer JournalEntry m ()
makeJournalProducer config journalStart =
  openJournal [LocalOnly] journalStart (Just $ upToPriority $ minPriority config) Nothing (mode config) >-> P.dropWhile discardProcessed
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

readJournalUploadState :: (MonadIO m) => FilePath -> ExceptT AppError m JournalUploadState
readJournalUploadState fp = ExceptT $ first StateMalformedError <$> liftIO (A.eitherDecodeFileStrict fp)

readJournalUploadStateCursor :: (MonadIO m) => FilePath -> ExceptT AppError m Start
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

papertrailUploadConfig :: ByteString -> Maybe JournalUploadConfig
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

-- uploadJson :: (MonadIO m, ToJSON a) => HTTP.Manager -> HTTP.Request -> a -> m ()
-- uploadJson httpManager baseRequest entry = do
--   let request = baseRequest {requestBody = RequestBodyLBS $ A.encode entry}
--   void $ liftIO $ HTTP.httpNoBody request httpManager

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
      u = decodeUtf8 $ fromMaybe "-" $ Map.lookup "SYSLOG_IDENTIFIER" fields
      pid = decodeUtf8 $ fromMaybe "-" $ Map.lookup "_PID" fields
      m = syslogStructuredData (journalFields config) entry
   in UTF8L.fromString $ printf "<22>%s %s %s %s %s - - [%s timestamp=%s]" p t h u pid m t

formatJournalEntryTime :: JournalEntry -> String
formatJournalEntryTime entry =
  let (seconds, micros) = toInteger (journalEntryRealtime entry) `divMod` (1000 * 1000)
      timestampUTCTime = addUTCTime (fromInteger micros / (1000 * 1000)) (posixSecondsToUTCTime (fromInteger seconds))
   in formatISO8601Micros timestampUTCTime

httpRetryPolicy :: Monad m => RetryPolicyM m
httpRetryPolicy = capDelay (60 * 1000 * 1000) (exponentialBackoff (1000 * 1000))

retryHttp :: (MonadIO m, MonadMask m) => (RetryStatus -> m a) -> m a
retryHttp = recovering httpRetryPolicy [handleHttpException]
  where
    handleHttpException _ =
      Handler
        ( \(ex :: HTTP.HttpException) ->
            let res = case ex of
                  HTTP.HttpExceptionRequest _ content -> case content of
                    HTTP.ResponseTimeout -> True
                    HTTP.ConnectionTimeout -> True
                    HTTP.ConnectionFailure _ -> True
                    _ -> False
                  _ -> False
             in fmap (const res) $ liftIO $ putStrLn ("Received " ++ show ex ++ ", retry=" ++ show res)
        )

processJournalEntry :: (MonadIO m) => HTTP.Manager -> FilePath -> JournalUploadConfig -> Request -> JournalEntry -> m ()
processJournalEntry httpManager stateFilePath config baseRequest journalEntry = do
  -- liftIO $ print journalEntry
  let body = encodeJournalFields config journalEntry
  -- liftIO $ print body
  void $ liftIO $ retryHttp $ const (HTTP.httpNoBody (baseRequest {requestBody = RequestBodyLBS body}) httpManager)
  liftIO $ writeJournalUploadState stateFilePath JournalUploadState {lastCursor = SerializableJournalEntryCursor $ journalEntryCursor journalEntry}

makeJournalUploader :: (MonadIO m) => FilePath -> JournalUploadConfig -> IO (JournalEntry -> m ())
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

data AppError
  = StateMalformedError String
  | DestinationURIInvalid
  | TokenUnavailable
  deriving (Show)

runWithConfig :: (MonadIO m, MonadMask m) => JournalUploadConfig -> ExceptT AppError m ()
runWithConfig config = do
  stateFilePath <- liftIO $ getJournalStateFilePath "default"
  journalUploader <- liftIO $ makeJournalUploader stateFilePath config
  start <- getJournalStart stateFilePath
  let journalProducer = makeJournalProducer config start
  runSafeT $ runEffect $ journalProducer >-> P.mapM_ journalUploader

runPapertrail :: (MonadIO m, MonadMask m) => ExceptT AppError m ()
runPapertrail = do
  liftIO $ logInfo "Starting"
  maybeToken <- liftIO $ lookupEnv "PAPERTRAIL_TOKEN"
  token <- maybe (except $ Left TokenUnavailable) (pure . UTF8.fromString) maybeToken
  config <- maybe (except $ Left DestinationURIInvalid) pure $ papertrailUploadConfig token
  runWithConfig config

main :: IO ()
main = runExceptT runPapertrail >>= either (error . show) pure
