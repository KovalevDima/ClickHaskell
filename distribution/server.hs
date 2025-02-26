{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}

import ClickHaskell
  ( ChString, ChUInt32
  , Column, Table
  , WritableInto, insertInto
  , Connection, openNativeConnection, defaultCredentials
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Exception (SomeException, catch)
import Control.Monad (filterM, forM, forever, void)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString.Lazy as B (LazyByteString, readFile)
import Data.HashMap.Strict as HM (HashMap, empty, fromList, lookup, unions)
import Data.Text as T (pack)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), bind, listen, maxListenQueue, socket)
import Network.Wai (Request (..), Response, ResponseReceived, responseLBS)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, runSettingsSocket, setPort)
import System.Directory (doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropFileName, dropTrailingPathSeparator, normalise, takeFileName, (</>))
import System.IO (stderr, hPutStrLn)

{-
```
CREATE TABLE ClickHaskellStats
(
    `path` LowCardinality(String),
    `remoteAddr` UInt32
)
ENGINE = MergeTree
PARTITION BY (path)
ORDER BY (path);
```
-}

main :: IO ()
main = do
  mSocketPath  <- lookupEnv "CLICKHASKELL_PAGE_SOCKET_PATH"
  mStaticFiles <- lookupEnv "CLICKHASKELL_STATIC_FILES_DIR"

  staticFiles   <- maybe (pure HM.empty) (flip withCurrentDirectory (listFilesWithContents ".")) mStaticFiles
  docsStatQueue <- newTBQueueIO 100_000
  clickHouse    <- openNativeConnection defaultCredentials

  serverTask <- async $ runServer MkServerState{..} mSocketPath
  databaseWriter <- async $
    forever $ do
      catch
        @SomeException
        (writeStats MkDocsStatisticsState{..})
        print
      threadDelay 5_000_000
  void $ waitAnyCancel [serverTask, databaseWriter]

data DocsStatistics = MkDocsStatistics
  { path       :: StrictByteString
  , remoteAddr :: Word32
  }
  deriving stock (Generic)
  deriving anyclass (WritableInto DocsStatTable)

type DocsStatTable = 
  Table 
    "ClickHaskellStats"
   '[ Column "path" ChString
    , Column "remoteAddr" ChUInt32
    ]




{-

Stats handler

-}

data DocsStatisticsState = MkDocsStatisticsState
  { docsStatQueue :: TBQueue DocsStatistics
  , clickHouse :: Connection
  }

writeStats :: DocsStatisticsState -> IO ()
writeStats MkDocsStatisticsState{..} = do
  dataToWrite <- (atomically . flushTBQueue) docsStatQueue
  insertInto @DocsStatTable clickHouse dataToWrite




{-

Web application core logic

-}

data ServerState = MkServerState
  { staticFiles :: HashMap FilePath (MimeType, LazyByteString)
  , docsStatQueue :: TBQueue DocsStatistics
  , clickHouse :: Connection
  }

runServer :: ServerState -> Maybe FilePath -> IO ()
runServer serverState mSocketPath = do
  case SockAddrUnix <$> mSocketPath of
    Nothing -> do
      let port = 3000 :: Port
      hPutStrLn stderr $ "Starting server on http://localhost:" <> show port
      runSettings (setPort port defaultSettings) (app serverState)
    Just sockAddr -> do
      sock <- socket AF_UNIX Stream 0
      hPutStrLn stderr $ "Starting server on UNIX socket: " ++ (show sockAddr)
      bind sock sockAddr
      listen sock maxListenQueue
      runSettingsSocket defaultSettings sock (app serverState)


app :: ServerState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app MkServerState{staticFiles, docsStatQueue} req f = do
  let path = (dropIndexHtml . BS8.unpack . rawPathInfo) req
      remoteAddr = case remoteHost req of SockAddrInet _ ip -> ip; _ -> 0
  case HM.lookup path staticFiles of
    Nothing -> f (responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found")
    Just (mimeType, content) -> do
      hPutStrLn stderr ("Remote host is: " <> show (remoteHost req))
      (atomically . writeTBQueue docsStatQueue)
        MkDocsStatistics
          { path       = BS8.pack path
          , remoteAddr = remoteAddr
          }
      f (responseLBS status200 [(hContentType, mimeType)] content)


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
