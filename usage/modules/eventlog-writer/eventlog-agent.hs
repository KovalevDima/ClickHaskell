{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import ClickHaskell
import Control.Exception (SomeException, bracketOnError, catch, finally)
import Control.Monad (forever, void)
import Data.ByteString as BS (ByteString, length)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEvents, decodeHeader)
import Network.Socket
import Network.Socket.ByteString (recv)
import System.Timeout (timeout)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  maybe mempty (chEventlogWrite undefined)
    =<< lookupEnv "CLICKHASKELL_EVENTLOG_SOCKET_PATH"

chEventlogWrite :: IO Connection -> FilePath -> IO ()
chEventlogWrite _initConn socketPath = do
  sock <-
    maybe (error "Socket connection timeout") pure
      =<< timeout 3_000_000 (
        bracketOnError
          (socket AF_UNIX Stream 0)
          (\sock ->
            catch @SomeException
              (finally (shutdown sock ShutdownBoth) (close sock))
              (const $ pure ())
          )
          (\sock -> do
            connect sock (SockAddrUnix socketPath)
            pure sock
          )
        )
  streamFromSocket sock

streamFromSocket :: Socket -> IO ()
streamFromSocket sock = do
  buffer <- initBuffer sock
  header <- loop buffer decodeHeader
  print header
  void . forever $ do
    print =<< loop buffer (decodeEvents header)
  where
  loop buff decoder = case decoder of
    Produce res (Done left) -> writeToBuffer buff left *> pure res
    Produce res dec -> print res *> loop buff dec
    Consume dec     -> loop buff . dec =<< readBuffer buff
    Error _bs err -> error err
    Done _ -> error "Unexpected done"

data Buffer = MkBuffer {bufferSocket :: Socket, buff :: IORef ByteString}

initBuffer :: Socket -> IO Buffer
initBuffer sock = MkBuffer sock <$> newIORef ""

flushBuffer :: Buffer -> IO ()
flushBuffer MkBuffer{buff} = atomicWriteIORef buff ""

writeToBuffer :: Buffer -> BS.ByteString -> IO ()
writeToBuffer MkBuffer{..} val = void (atomicModifyIORef buff (val,))

readBuffer :: Buffer -> IO BS.ByteString
readBuffer buffer@MkBuffer{..} =
  readIORef buff
    >>= (\currentBuffer ->
      case BS.length currentBuffer of
        0 -> recv bufferSocket 4096
        _ -> flushBuffer buffer *> pure currentBuffer
    )

type EventlogTable = 
  Table 
    "haskell_eventlog"
   '[ Column "time" (DateTime "")
    ]
