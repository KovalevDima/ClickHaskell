{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}

import ClickHaskell
  ( ChString, DateTime
  , UInt16, UInt32, UInt64
  , WritableInto, insertInto
  , Connection, openNativeConnection, defaultCredentials
  , ReadableFrom, Column, Table
  , View, selectFromView, Parameter, parameter, ChCredential
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (TBQueue, TChan, TVar, atomically, dupTChan, flushTBQueue, newBroadcastTChanIO, newTBQueueIO, newTVarIO, readTChan, readTVarIO, writeTBQueue, writeTChan, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (filterM, forM, forever)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString.Lazy as B (LazyByteString, readFile)
import Data.HashMap.Strict as HM (HashMap, empty, fromList, lookup, unions)
import Data.Text as T (pack)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stats (RTSStats (..), getRTSStats)
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


main :: IO ()
main = do
  mSocketPath  <- lookupEnv "CLICKHASKELL_PAGE_SOCKET_PATH"
  mStaticFiles <- lookupEnv "CLICKHASKELL_STATIC_FILES_DIR"

  docsStatQueue  <- newTBQueueIO 100_000
  broadcastChan  <- newBroadcastTChanIO

  (visitsCollector, currentHistory) <- initVisitsTracker MkDocsStatisticsArgs{..}
  perfStatCollector <- initPerformanceTracker defaultCredentials

  server <-
    let mkBroadcastChan = atomically (dupTChan broadcastChan)
    in initServer MkServerArgs{..}

  runConcurrently
    $ pure ()
    *> visitsCollector
    *> perfStatCollector
    *> server

{-

  * Performance stats handler

-}

initPerformanceTracker :: ChCredential -> IO (Concurrently())
initPerformanceTracker cred = do
  _perfChConnection <- openNativeConnection cred
  performanceQueue <- newTBQueueIO 100

  pure $ 
    Concurrently (forever $ do
      atomically . writeTBQueue performanceQueue . (\RTSStats{..} -> MkPerformanceStat{..})
        =<< getRTSStats
      threadDelay 1_000_000
    )
    *>
    Concurrently (forever $ do
      _ <- atomically (flushTBQueue performanceQueue)
      threadDelay 10_000_000
    )

data PerformanceStat = MkPerformanceStat
  { max_live_bytes :: UInt64
  }
  deriving (Generic)

{-

  * Visits stats handler

-}

data DocsStatisticsArgs = MkDocsStatisticsArgs
  { docsStatQueue  :: TBQueue DocsStatistics
  , broadcastChan  :: TChan HistoryData
  }

initVisitsTracker :: DocsStatisticsArgs -> IO (Concurrently (), TVar HistoryData)
initVisitsTracker MkDocsStatisticsArgs{..} = do
  clickHouse     <- openNativeConnection defaultCredentials
  currentHistory <- newTVarIO . History =<< readCurrentHistoryLast clickHouse 24

  pure
    ( Concurrently (forever $ do
        catch (do
            dataToWrite <- (atomically . flushTBQueue) docsStatQueue
            insertInto @DocsStatTable clickHouse dataToWrite
          )
          (print @SomeException)
        threadDelay 5_000_000
      )
      *>
      Concurrently (forever $ do
        historyByHours <- readCurrentHistoryLast clickHouse 1
        case historyByHours of
          update:_ -> atomically $ (writeTChan broadcastChan) (HistoryUpdate update)
          _        -> pure ()
        threadDelay 1_000_000
      )
      *>
      Concurrently (forever $ do
        atomically . writeTVar currentHistory . History =<< readCurrentHistoryLast clickHouse 24
        threadDelay 300_000_000
      )
    , currentHistory
    )

readCurrentHistoryLast :: Connection -> UInt16 -> IO [HourData]
readCurrentHistoryLast clickHouse hours =
  concat <$>
    selectFromView
      @HistoryByHours
      clickHouse
      (parameter @"hoursLength" @UInt16 hours)
      pure

-- ** Writing data

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
   '[ Column "time" (DateTime "")
    , Column "path" ChString
    , Column "remoteAddr" UInt32
    ]

-- ** Reading data

data HourData = MkHourData
  { hour :: Word32
  , visits :: Word32
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ReadableFrom HistoryByHours)

type HistoryByHours =
  View
    "historyByHours"
   '[ Column "hour" UInt32
    , Column "visits" UInt32
    ]
   '[ Parameter "hoursLength" UInt16
    ]

{-

  * Web application core logic

-}

type StaticFiles = HashMap StrictByteString (MimeType, LazyByteString)

data ServerArgs = MkServerArgs
  { mStaticFiles    :: Maybe String
  , mSocketPath     :: Maybe FilePath
  , docsStatQueue   :: TBQueue DocsStatistics
  , currentHistory  :: TVar HistoryData
  , mkBroadcastChan :: IO (TChan HistoryData)
  }

data HistoryData = History{history :: [HourData]} | HistoryUpdate{realtime :: HourData}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

initServer :: ServerArgs -> IO (Concurrently())
initServer args@MkServerArgs{mStaticFiles, mSocketPath} = do
  staticFiles <-
    maybe
      (pure HM.empty)
      (flip withCurrentDirectory (listFilesWithContents "."))
      mStaticFiles

  let
    app = websocketsOr
      defaultConnectionOptions
      (wsServer args)
      (httpApp args staticFiles)

  pure $
    Concurrently $ do
      case SockAddrUnix <$> mSocketPath of
        Nothing -> do
          let port = 3000 :: Port
          putStrLn $ "Starting server on http://localhost:" <> show port
          runSettings (setPort port defaultSettings) app
        Just sockAddr -> do
          sock <- socket AF_UNIX Stream 0
          putStrLn $ "Starting server on UNIX socket: " ++ (show sockAddr)
          bind sock sockAddr
          listen sock maxListenQueue
          runSettingsSocket defaultSettings sock app


httpApp :: ServerArgs -> StaticFiles -> Application
httpApp MkServerArgs{docsStatQueue} staticFiles req f = do
  currentTime <- getCurrentTime
  let path       = (dropIndexHtml . BS8.unpack . rawPathInfo) req
      remoteAddr = maybe 0 getIPv4 (decodeUtf8 =<< Prelude.lookup "X-Real-IP" (requestHeaders req))
      time       = (floor . utcTimeToPOSIXSeconds) currentTime
  case HM.lookup path staticFiles of
    Nothing -> f (responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found")
    Just (mimeType, content) -> do
      (atomically . writeTBQueue docsStatQueue) MkDocsStatistics{..}
      f (responseLBS status200 [(hContentType, mimeType)] content)

wsServer :: ServerArgs -> ServerApp
wsServer MkServerArgs{mkBroadcastChan, currentHistory} pending = do
  conn <- acceptRequest pending
  sendTextData conn =<< (encode <$> readTVarIO currentHistory)
  clientChan <- mkBroadcastChan
  forever $ do
    sendTextData conn =<< (fmap encode . atomically . readTChan) clientChan

listFilesWithContents :: FilePath -> IO (HashMap StrictByteString (MimeType, LazyByteString))
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
  prepareFilePath :: FilePath -> StrictByteString
  prepareFilePath = dropIndexHtml . normalise . ("/" </>)  


dropIndexHtml :: FilePath -> StrictByteString
dropIndexHtml fp = BS8.pack .  dropTrailingPathSeparator $
  if takeFileName fp == "index.html"
  then dropFileName fp
  else fp


{-
<pre><code class="sql" data-lang="sql"
>CREATE TABLE default.ClickHaskellStats
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
</code></pre>
-}
