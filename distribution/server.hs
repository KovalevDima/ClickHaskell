{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}

import ClickHaskell
  ( ChString, DateTime
  , UInt16, UInt32
  , WritableInto, insertInto
  , Connection, openNativeConnection, defaultCredentials
  , ReadableFrom, Column, Table
  , View, selectFromView, Parameter, parameter
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
import Data.Time (getCurrentTime)
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
  currentHistory <- newTVarIO . History =<< readCurrentHistoryLast clickHouse 24

  broadcastingJob <- async $ forever $ do
    historyByHours <- readCurrentHistoryLast clickHouse 1
    case historyByHours of
      update:_ -> atomically $ (writeTChan broadcastChan) (HistoryUpdate update)
      _        -> pure ()
    threadDelay 1_000_000
  updateCurrentHistory <- async $ forever $ do
    atomically . writeTVar currentHistory . History =<< readCurrentHistoryLast clickHouse 24
    threadDelay 300_000_000
  serverTask <- async $ runServer MkServerState{..} mSocketPath
  databaseWriter <- async $ forever $ do
    writeStats MkDocsStatisticsState{..}
    threadDelay 5_000_000
  void $ waitAnyCancel [serverTask, databaseWriter, broadcastingJob, updateCurrentHistory]

{-

  * Stats handler

-}

data DocsStatisticsState = MkDocsStatisticsState
  { docsStatQueue  :: TBQueue DocsStatistics
  , clickHouse     :: ClickHaskell.Connection
  }

readCurrentHistoryLast :: Connection -> UInt16 -> IO [HourData]
readCurrentHistoryLast clickHouse hours =
  concat <$>
    selectFromView
      @HistoryByHours
      clickHouse
      (parameter @"hoursLength" @UInt16 hours)
      pure

writeStats :: DocsStatisticsState -> IO ()
writeStats MkDocsStatisticsState{..} = catch
  ( do
    dataToWrite <- (atomically . flushTBQueue) docsStatQueue
    insertInto @DocsStatTable clickHouse dataToWrite
  )
  (print @SomeException)

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

data ServerState = MkServerState
  { staticFiles    :: HashMap StrictByteString (MimeType, LazyByteString)
  , docsStatQueue  :: TBQueue DocsStatistics
  , currentHistory :: TVar HistoryData
  , broadcastChan  :: TChan HistoryData
  }

data HistoryData = History{history :: [HourData]} | HistoryUpdate{realtime :: HourData}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

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
  websocketsOr
    defaultConnectionOptions
    (wsServer state) (httpApp state)
    req respond

httpApp :: ServerState -> Application
httpApp MkServerState{staticFiles, docsStatQueue} req f = do
  currentTime <- getCurrentTime
  let path       = (dropIndexHtml . BS8.unpack . rawPathInfo) req
      remoteAddr = maybe 0 getIPv4 (decodeUtf8 =<< Prelude.lookup "X-Real-IP" (requestHeaders req))
      time       = (floor . utcTimeToPOSIXSeconds) currentTime
  case HM.lookup path staticFiles of
    Nothing -> f (responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found")
    Just (mimeType, content) -> do
      (atomically . writeTBQueue docsStatQueue) MkDocsStatistics{..}
      f (responseLBS status200 [(hContentType, mimeType)] content)

wsServer :: ServerState -> ServerApp
wsServer MkServerState{broadcastChan, currentHistory} pending = do
  conn <- acceptRequest pending
  sendTextData conn =<< (encode <$> readTVarIO currentHistory)
  clientChan <- atomically $ dupTChan broadcastChan
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
