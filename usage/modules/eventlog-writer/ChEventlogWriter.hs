{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ChEventlogWriter where

import ClickHaskell (Connection)
import Control.Exception (SomeException, bracketOnError, catch, finally)
import GHC.Eventlog.Socket (start)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog, decodeHeader, decodeEvents)
import Network.Socket
import Network.Socket.ByteString (recv)
import System.Timeout (timeout)
import Debug.Trace (traceShowId)
import GHC.RTS.Events (Header, Event)
import Data.ByteString as BS (ByteString, length)
import Data.IORef (IORef, readIORef, atomicWriteIORef, newIORef, atomicModifyIORef)
import Control.Monad (void)

chEventlogWrite :: IO Connection -> FilePath -> IO ()
chEventlogWrite _initConn socketPath = do
  start socketPath
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
  events <- loop buffer (decodeEvents header)
  print events
  pure ()
  where
  loop buff decoder = case decoder of
    Produce res (Done left) -> writeToBuffer buff left *> pure res
    Consume dec     -> loop buff . dec =<< readBuffer buff
    Produce _ _dec  -> error "Got header with unexpected decoder state"
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
