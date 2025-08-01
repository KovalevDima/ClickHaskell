{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}

import ChVisits (DocsStatistics (..), DocsStatisticsArgs (..), HistoryData, initVisitsTracker)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (TBQueue, TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTBQueueIO, readTChan, readTVarIO, writeTBQueue)
import Control.Monad (filterM, forM, forever)
import Data.Aeson (encode)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString.Lazy as B (LazyByteString, readFile)
import Data.HashMap.Strict as HM (HashMap, empty, fromList, lookup, unions)
import Data.Maybe (isJust)
import Data.Text as T (pack)
import Data.Time (getCurrentTime)
import Debug.Trace (traceEventIO)
import GHC.Eventlog.Socket (start)
import Net.IPv4 (decodeUtf8, getIPv4)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), bind, listen, maxListenQueue, socket)
import Network.Wai (Application, Request (..), responseLBS)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, runSettingsSocket, setPort)
import Network.Wai.Handler.WebSockets (websocketsApp)
import Network.WebSockets as WebSocket (ServerApp, acceptRequest, sendTextData)
import Network.WebSockets.Connection (defaultConnectionOptions)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropFileName, dropTrailingPathSeparator, normalise, replaceExtension, takeExtension, takeFileName, (</>))


main :: IO ()
main = do
  maybe mempty start
    =<< lookupEnv "EVENTLOG_SOCKET_PATH"

  mSocketPath  <- lookupEnv "CLICKHASKELL_PAGE_SOCKET_PATH"
  mStaticFiles <- lookupEnv "CLICKHASKELL_STATIC_FILES_DIR"
  isDev        <- isJust <$> lookupEnv "DEV"

  docsStatQueue  <- newTBQueueIO 100_000
  broadcastChan  <- newBroadcastTChanIO

  (visitsCollector, currentHistory) <- initVisitsTracker MkDocsStatisticsArgs{..}

  server <-
    let mkBroadcastChan = atomically (dupTChan broadcastChan)
    in initServer MkServerArgs{..}

  runConcurrently
    $ pure ()
    *> visitsCollector
    *> server


{-

  * Web application core logic

-}

type StaticFiles = HashMap StrictByteString (MimeType, IO LazyByteString)

data ServerArgs = MkServerArgs
  { isDev           :: Bool
  , mStaticFiles    :: Maybe String
  , mSocketPath     :: Maybe FilePath
  , docsStatQueue   :: TBQueue DocsStatistics
  , currentHistory  :: TVar HistoryData
  , mkBroadcastChan :: IO (TChan HistoryData)
  }

initServer :: ServerArgs -> IO (Concurrently())
initServer args@MkServerArgs{mStaticFiles, mSocketPath, isDev, docsStatQueue} = do
  staticFiles <-
    maybe
      (pure HM.empty)
      (listFilesWithContents isDev)
      mStaticFiles

  let
    app = \req respond ->
      case websocketsApp defaultConnectionOptions (wsServer args) req of
        Nothing  -> httpApp args staticFiles req respond
        Just res -> do
          time <- getCurrentTime
          let remoteAddr = maybe 0 getIPv4 (decodeUtf8 =<< Prelude.lookup "X-Real-IP" (requestHeaders req))
              path       = rawPathInfo req
          (atomically . writeTBQueue docsStatQueue) MkDocsStatistics{..}
          respond res

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
httpApp _ staticFiles req f = do
  traceEventIO "http"
  case routeSPA staticFiles (rawPathInfo req) of
    Nothing -> f (responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found")
    Just (mimeType, content) -> do
      f . responseLBS status200 [(hContentType, mimeType)] =<< content

wsServer :: ServerArgs -> ServerApp
wsServer MkServerArgs{mkBroadcastChan, currentHistory} pending = do
  conn <- acceptRequest pending
  sendTextData conn =<< (encode <$> readTVarIO currentHistory)
  clientChan <- mkBroadcastChan
  traceEventIO "wsInit"
  forever $ do
    sendTextData conn =<< (fmap encode . atomically . readTChan) clientChan

listFilesWithContents :: Bool -> FilePath -> IO StaticFiles
listFilesWithContents isDev dir = go "."
  where
  go subPath = do
    paths <- map (subPath </>) <$> listDirectory (dir </> subPath)
    files <- (`filterM` paths) $ \path ->
      (&&)
        <$> (fmap not . doesDirectoryExist) (dir </> path)
        <*> (pure . isDocFile) (dir </> path)
    fileContents <- forM files $ \file -> do
      content <- B.readFile (dir </> file)
      let contentLoader = if isDev then B.readFile (dir </> file) else pure content
      return
        ( prepareFilePath file
        , (defaultMimeLookup (T.pack $ filePathToUrlPath file), contentLoader)
        )
    nestedMaps <- mapM go =<< filterM (\path -> doesDirectoryExist (dir </> path)) paths
    return $ HM.unions (HM.fromList fileContents : nestedMaps)

  isDocFile :: FilePath -> Bool
  isDocFile fp
    | takeExtension fp `elem` [".html", ".lhs", ".ttf", ".svg", ".css", ".js"] = True
    | otherwise              = False

  prepareFilePath :: FilePath -> StrictByteString
  prepareFilePath = dropIndexHtml . filePathToUrlPath . normalise . ("/" </>)

  filePathToUrlPath :: FilePath -> FilePath
  filePathToUrlPath fp
    | takeExtension fp == ".lhs" = replaceExtension fp "html"
    | otherwise = fp

routeSPA :: StaticFiles -> StrictByteString -> Maybe (MimeType, IO LazyByteString)
routeSPA staticFiles rawPath =
  let path = (dropIndexHtml . BS8.unpack) rawPath
  in HM.lookup path staticFiles <|> HM.lookup "/" staticFiles

dropIndexHtml :: FilePath -> StrictByteString
dropIndexHtml fp = BS8.pack .  dropTrailingPathSeparator $
  if takeFileName fp == "index.html"
  then dropFileName fp
  else fp
