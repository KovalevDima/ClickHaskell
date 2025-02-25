{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TBQueue
import Control.Monad (filterM, forM, forever)
import Data.ByteString.Char8 as BS8 (unpack)
import Data.ByteString.Lazy as B hiding (map)
import Data.HashMap.Strict as HM hiding (map)
import Data.Text as T (pack)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), bind, socket, listen, maxListenQueue)
import Network.Wai (Request (..), Response, ResponseReceived, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, runSettingsSocket, setPort)
import System.Directory (doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropFileName, dropTrailingPathSeparator, normalise, takeFileName, (</>))


main :: IO ()
main = do
  mSocketPath  <- lookupEnv "CLICKHASKELL_PAGE_SOCKET_PATH"
  mStaticFiles <- lookupEnv "CLICKHASKELL_STATIC_FILES_DIR"

  staticFiles <- maybe (pure HM.empty) (flip withCurrentDirectory (listFilesWithContents ".")) mStaticFiles
  docsStatQueue <- newTBQueueIO 100_000
  case SockAddrUnix <$> mSocketPath of
    Nothing ->
      runSettings
        (setPort 3000 $ defaultSettings)
        (app MkServerState{..})
    Just sockAddr -> do
      sock <- socket AF_UNIX Stream 0
      putStrLn $ "Starting server on UNIX socket: " ++ (show sockAddr)
      bind sock sockAddr
      listen sock maxListenQueue
      runSettingsSocket defaultSettings sock (app MkServerState{..})

  forever $ threadDelay 60_000_000

data DocsStat


data ServerState = MkServerState
  { staticFiles :: HashMap FilePath (MimeType, LazyByteString)
  , docsStatQueue :: TBQueue DocsStat
  }


app :: ServerState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app MkServerState{staticFiles} req f = do
  let path = dropIndexHtml . BS8.unpack $ rawPathInfo req
  case HM.lookup path staticFiles of
    Nothing -> f (responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found")
    Just (mimeType, content) -> do
      f (responseLBS status200 [(hContentType, mimeType)] content)


listFilesWithContents :: FilePath -> IO (HashMap FilePath (MimeType, ByteString))
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
