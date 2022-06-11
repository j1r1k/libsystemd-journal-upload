{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Systemd.Journal.Upload where

import Control.Logger.Simple (logInfo)
import Control.Monad (void)
import Control.Monad.Catch (Handler (Handler))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry (RetryPolicyM, RetryStatus, capDelay, exponentialBackoff, recovering)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser, parserThrowError, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8L (fromString)
import Data.Functor (($>))
import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Strict as Map (lookup, singleton, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate, pack, unpack)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.ISO8601 (formatISO8601Micros)
import GHC.Generics (Generic)
import Network.HTTP.Client (Request (checkResponse, method, requestBody, requestHeaders), RequestBody (RequestBodyLBS))
import qualified Network.HTTP.Client as HTTP (HttpException (HttpExceptionRequest), HttpExceptionContent (ConnectionFailure, ConnectionTimeout, ResponseTimeout), Manager, httpNoBody, newManager, requestFromURI, throwErrorStatusCodes)
import qualified Network.HTTP.Client.TLS as HTTP (tlsManagerSettings)
import Network.HTTP.Types (Header, Method, methodPost)
import Network.URI (URI, nullURI)
import Pipes (Producer, runEffect, (>->))
import qualified Pipes.Prelude as P (dropWhile, mapM_)
import Pipes.Safe (MonadMask, MonadSafe, runSafeT)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
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

newtype JournalUploadError = StateMalformedError String deriving (Show)

getJournalStateFilePath :: String -> IO FilePath
getJournalStateFilePath producerId = do
  dataDir <- getXdgDirectory XdgData $ "systemd" </> "journal-upload"
  createDirectoryIfMissing True dataDir
  return $ dataDir </> ("state" <.> producerId <.> "json")

getJournalStart :: (MonadIO m) => FilePath -> ExceptT JournalUploadError m Start
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

newtype JournalUploadState = JournalUploadState {lastCursor :: SerializableJournalEntryCursor} deriving (Eq, Generic, Show)

instance FromJSON JournalUploadState

instance ToJSON JournalUploadState

readJournalUploadState :: (MonadIO m) => FilePath -> ExceptT JournalUploadError m JournalUploadState
readJournalUploadState fp = ExceptT $ first StateMalformedError <$> liftIO (A.eitherDecodeFileStrict fp)

readJournalUploadStateCursor :: (MonadIO m) => FilePath -> ExceptT JournalUploadError m Start
readJournalUploadStateCursor fp = do
  JournalUploadState {lastCursor = SerializableJournalEntryCursor cursor} <- readJournalUploadState fp
  return $ FromCursor cursor Forwards

writeJournalUploadState :: FilePath -> JournalUploadState -> IO ()
writeJournalUploadState = A.encodeFile

pickJournalFields :: [(Text, Text)] -> JournalEntry -> HashMap Text Text
pickJournalFields fields journalEntry =
  foldMap (\(key, newKey) -> maybe mempty (Map.singleton newKey . decodeLatin1) $ Map.lookup (mkJournalField key) $ journalEntryFields journalEntry) fields

syslogStructuredData :: JournalFields -> JournalEntry -> Text
syslogStructuredData (JournalFields fields) entry = Text.intercalate " " $ fmap printPair $ Map.toList $ pickJournalFields fields entry
  where
    printPair :: (Text, Text) -> Text
    printPair (k, v) = k <> "=\"" <> v <> "\""

newtype JournalFields = JournalFields [(Text, Text)]

encodeSyslog :: JournalFields -> JournalEntry -> BL.ByteString
encodeSyslog journalFields entry =
  let t = formatJournalEntryTime entry
      fields = journalEntryFields entry
      p = decodeLatin1 $ fromMaybe "6" $ Map.lookup "PRIORITY" fields
      h = decodeLatin1 $ fromMaybe "-" $ Map.lookup "_HOSTNAME" fields
      u = decodeLatin1 $ fromMaybe "-" $ Map.lookup "SYSLOG_IDENTIFIER" fields
      pid = decodeLatin1 $ fromMaybe "-" $ Map.lookup "_PID" fields
      m = syslogStructuredData journalFields entry
   in UTF8L.fromString $ printf "<22>%s %s %s %s %s - - [%s timestamp=%s]" p t h u pid m t

data JournalUploadConfig = JournalUploadConfig
  { minPriority :: Priority,
    mode :: Mode,
    destinationUri :: URI,
    journalUploadRequestHeaders :: [Header],
    journalUploadRequestMethod :: Method,
    journalUploadFields :: JournalFields,
    journalUploadEncoder :: JournalFields -> JournalEntry -> BL.ByteString
  }

defaultJournalUploadConfig :: JournalUploadConfig
defaultJournalUploadConfig =
  JournalUploadConfig
    { minPriority = Priority.Warning,
      mode = Waiting,
      destinationUri = nullURI,
      journalUploadRequestHeaders = mempty,
      journalUploadRequestMethod = methodPost,
      journalUploadFields = JournalFields [("_UID", "uid"), ("MESSAGE", "message")],
      journalUploadEncoder = encodeSyslog
    }

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
             in fmap (const res) $ liftIO $ logInfo $ Text.pack ("Received " ++ show ex ++ ", retry=" ++ show res)
        )

processJournalEntry :: (MonadIO m) => HTTP.Manager -> FilePath -> JournalUploadConfig -> Request -> JournalEntry -> m ()
processJournalEntry httpManager stateFilePath config baseRequest journalEntry = do
  let uploadEncoder = journalUploadEncoder config
  let body = uploadEncoder (journalUploadFields config) journalEntry
  void $ liftIO $ retryHttp $ const (HTTP.httpNoBody (baseRequest {requestBody = RequestBodyLBS body}) httpManager)
  liftIO $ writeJournalUploadState stateFilePath JournalUploadState {lastCursor = SerializableJournalEntryCursor $ journalEntryCursor journalEntry}

makeJournalUploader :: (MonadIO m) => FilePath -> JournalUploadConfig -> IO (JournalEntry -> m ())
makeJournalUploader stateFilePath config = do
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  requestWithURI <- HTTP.requestFromURI (destinationUri config)
  let baseRequest =
        requestWithURI
          { method = journalUploadRequestMethod config,
            requestHeaders = journalUploadRequestHeaders config,
            checkResponse = HTTP.throwErrorStatusCodes
          }
  return $ processJournalEntry httpManager stateFilePath config baseRequest

runWithConfig :: (MonadIO m, MonadMask m) => JournalUploadConfig -> ExceptT JournalUploadError m ()
runWithConfig config = do
  stateFilePath <- liftIO $ getJournalStateFilePath "default"
  journalUploader <- liftIO $ makeJournalUploader stateFilePath config
  start <- getJournalStart stateFilePath
  let journalProducer = makeJournalProducer config start
  runSafeT $ runEffect $ journalProducer >-> P.mapM_ journalUploader
