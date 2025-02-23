{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM, forever)
import Data.ByteString.Char8 as BS8 (unpack)
import Data.ByteString.Lazy as B hiding (map)
import Data.HashMap.Strict as HM hiding (map)
import Data.Maybe (fromMaybe)
import Data.Text as T (pack)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Socket (SockAddr(..), Family(..), SocketType(..), socket, bind)
import Network.Wai (Request (..), Response, ResponseReceived, responseLBS)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings, runSettingsSocket)
import System.Directory (doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropFileName, dropTrailingPathSeparator, normalise, takeFileName, (</>))


main :: IO ()
main = do
  mSocketPath    <- lookupEnv "CLICKHASKELL_PAGE_SOCKET_PATH"
  staticFilesDir <- fromMaybe (error "") <$> lookupEnv "CLICKHASKELL_STATIC_FILES_DIR"
  staticFiles <- withCurrentDirectory staticFilesDir (listFilesWithContents ".")

  let settings = setPort 3000 $ defaultSettings
  case SockAddrUnix <$> mSocketPath of
    Nothing -> runSettings settings (app MkServerState{..})
    Just sockAddr -> do
      sock <- socket AF_UNIX Stream 0
      bind sock sockAddr
      runSettingsSocket settings sock (app MkServerState{..})

  forever $ threadDelay 60_000_000


data ServerState = MkServerState
  { staticFiles :: HashMap FilePath (MimeType, LazyByteString)
  }


app :: ServerState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app MkServerState{staticFiles} req f = do
  let path = dropIndexHtml . BS8.unpack $ rawPathInfo req
  f $ case HM.lookup path staticFiles of
    Just (mimeType, content) -> responseLBS status200 [(hContentType, mimeType)] content
    Nothing -> responseLBS status200 [(hContentType, "text/plain")] "Hello world"


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
