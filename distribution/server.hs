{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}

import ClickHaskell
  ( ChString, ChUInt32, ChDateTime
  , Column, Table
  , WritableInto, insertInto
  , Connection, openNativeConnection, defaultCredentials, View, selectFromView, Parameter, ChUInt16, parameter, ReadableFrom
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (TBQueue, TChan, atomically, dupTChan, flushTBQueue, newBroadcastTChanIO, newTBQueueIO, readTChan, writeTBQueue, writeTChan, TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (filterM, forM, forever, void)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString.Lazy as B (LazyByteString, readFile)
import Data.HashMap.Strict as HM (HashMap, empty, fromList, lookup, unions)
import Data.Text as T (pack)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Net.IPv4 (decodeUtf8, getIPv4)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), bind, listen, maxListenQueue, socket)
import Network.Wai (Application, Request (..), responseLBS)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, runSettingsSocket, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets as WebSocket (ServerApp, acceptRequest, sendTextData)
import Network.WebSockets.Connection (defaultConnectionOptions)
import System.Directory (doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropFileName, dropTrailingPathSeparator, normalise, takeFileName, (</>))

{-
```sql
CREATE TABLE default.ClickHaskellStats
(
    `time` DateTime,
    `path` LowCardinality(String),
    `remoteAddr` UInt32
)
ENGINE = MergeTree
PARTITION BY path
ORDER BY path
SETTINGS index_granularity = 8192;

CREATE OR REPLACE VIEW default.historyByHours
AS SELECT
    toUInt32(intDiv(toUInt32(time), 3600) * 3600) AS hour,
    toUInt32(countDistinct(remoteAddr)) AS visits
FROM default.ClickHaskellStats
WHERE hour > (now() - ({hoursLength:UInt16} * 3600))
GROUP BY hour
ORDER BY hour ASC;
```
-}

main :: IO ()
main = do
  mSocketPath  <- lookupEnv "CLICKHASKELL_PAGE_SOCKET_PATH"
  mStaticFiles <- lookupEnv "CLICKHASKELL_STATIC_FILES_DIR"

  staticFiles <-
    maybe
      (pure HM.empty)
      (flip withCurrentDirectory (listFilesWithContents "."))
      mStaticFiles
  docsStatQueue  <- newTBQueueIO 100_000
  clickHouse     <- openNativeConnection defaultCredentials
  broadcastChan  <- newBroadcastTChanIO
  let readCurrentHistoryLast (hours :: ChUInt16) =
        concat <$>
          selectFromView
            @HistoryByHours
            clickHouse
            (parameter @"hoursLength" @ChUInt16 hours)
            pure
  currentHistory <- newTVarIO . History =<< readCurrentHistoryLast 24

  broadcastingJob <- async $ forever $ do
    historyByHours <- readCurrentHistoryLast 1
    case historyByHours of
      update:_ -> atomically $ (writeTChan broadcastChan) (HistoryUpdate update)
      _        -> pure ()
    threadDelay 1_000_000
  updateCurrentHistory <- async $ forever $ do
    atomically . writeTVar currentHistory . History =<< readCurrentHistoryLast 24
    threadDelay 300_000_000
  serverTask <- async $ runServer MkServerState{..} mSocketPath
  databaseWriter <- async $ forever $ do
    catch
      (writeStats MkDocsStatisticsState{..})
      (print @SomeException)
    threadDelay 5_000_000
  void $ waitAnyCancel [serverTask, databaseWriter, broadcastingJob, updateCurrentHistory]

data DocsStatistics = MkDocsStatistics
  { time       :: Word32
  , path       :: StrictByteString
  , remoteAddr :: Word32
  }
  deriving stock (Generic)
  deriving anyclass (WritableInto DocsStatTable)

type DocsStatTable = 
  Table 
    "ClickHaskellStats"
   '[ Column "time" (ChDateTime "")
    , Column "path" ChString
    , Column "remoteAddr" ChUInt32
    ]

type HistoryByHours =
  View
    "historyByHours"
   '[ Column "hour" ChUInt32
    , Column "visits" ChUInt32
    ]
   '[ Parameter "hoursLength" ChUInt16
    ]

{-

Stats handler

-}

data DocsStatisticsState = MkDocsStatisticsState
  { docsStatQueue  :: TBQueue DocsStatistics
  , clickHouse     :: ClickHaskell.Connection
  , broadcastChan  :: TChan HistoryData
  }

writeStats :: DocsStatisticsState -> IO ()
writeStats MkDocsStatisticsState{..} = do
  dataToWrite <- (atomically . flushTBQueue) docsStatQueue
  insertInto @DocsStatTable clickHouse dataToWrite

{-

Web application core logic

-}

data ServerState = MkServerState
  { staticFiles    :: HashMap FilePath (MimeType, LazyByteString)
  , docsStatQueue  :: TBQueue DocsStatistics
  , clickHouse     :: ClickHaskell.Connection
  , currentHistory :: TVar HistoryData
  , broadcastChan  :: TChan HistoryData
  }

runServer :: ServerState -> Maybe FilePath -> IO ()
runServer serverState mSocketPath = do
  case SockAddrUnix <$> mSocketPath of
    Nothing -> do
      let port = 3000 :: Port
      putStrLn $ "Starting server on http://localhost:" <> show port
      runSettings (setPort port defaultSettings) (app serverState)
    Just sockAddr -> do
      sock <- socket AF_UNIX Stream 0
      putStrLn $ "Starting server on UNIX socket: " ++ (show sockAddr)
      bind sock sockAddr
      listen sock maxListenQueue
      runSettingsSocket defaultSettings sock (app serverState)


app :: ServerState -> Application
app state req respond = do
  time <- getCurrentTime
  websocketsOr
    defaultConnectionOptions
    (wsServer state)
    (httpApp state time)
    req
    respond

httpApp :: ServerState -> UTCTime -> Application
httpApp MkServerState{staticFiles, docsStatQueue} currentTime req f = do
  let path      = (dropIndexHtml . BS8.unpack . rawPathInfo) req
      clientIp4 = maybe 0 getIPv4 (decodeUtf8 =<< Prelude.lookup "X-Real-IP" (requestHeaders req))
      time      = (floor . utcTimeToPOSIXSeconds) currentTime
  case HM.lookup path staticFiles of
    Nothing -> f (responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found")
    Just (mimeType, content) -> do
      (atomically . writeTBQueue docsStatQueue)
        MkDocsStatistics
          { time
          , path       = BS8.pack path
          , remoteAddr = clientIp4
          }
      f (responseLBS status200 [(hContentType, mimeType)] content)

data HistoryData
  = History { history :: [HourData] }
  | HistoryUpdate { realtime :: HourData}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data HourData = MkHourData
  { hour :: Word32
  , visits :: Word32
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ReadableFrom HistoryByHours)

wsServer :: ServerState -> ServerApp
wsServer MkServerState{broadcastChan, currentHistory} pending = do
  clientChan <- atomically $ dupTChan broadcastChan
  conn <- acceptRequest pending
  currentHistoryState <- readTVarIO currentHistory
  sendTextData conn (encode currentHistoryState)
  forever $ do
    msg <- atomically $ readTChan clientChan
    sendTextData conn (encode msg)

listFilesWithContents :: FilePath -> IO (HashMap FilePath (MimeType, LazyByteString))
listFilesWithContents dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries
  subdirs <- filterM doesDirectoryExist paths
  files <- filterM (fmap not . doesDirectoryExist) paths
  fileContents <- forM files $ \file -> do
    content <- B.readFile file
    return (prepareFilePath file, (defaultMimeLookup (T.pack file), content))
  nestedMaps <- forM subdirs listFilesWithContents
  return $ HM.unions (HM.fromList fileContents : nestedMaps)
  where
  prepareFilePath :: FilePath -> FilePath
  prepareFilePath = dropIndexHtml . normalise . ("/" </>)  


dropIndexHtml :: FilePath -> FilePath
dropIndexHtml fp = dropTrailingPathSeparator $
  if takeFileName fp == "index.html"
  then dropFileName fp
  else fp
