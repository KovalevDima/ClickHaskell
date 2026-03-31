{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module EventLogAgent.EventlogNetworking where

import Control.Concurrent.Async
import Control.Concurrent.STM (newTBQueueIO, writeTBQueue, atomically, flushTBQueue)
import Control.Exception
import Control.Monad (forever, void)
import Data.ByteString as BS (ByteString, length, null)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import Data.Time (UTCTime, getCurrentTime)
import GHC.RTS.Events (Event, Header)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEvents, decodeHeader)
import Network.Socket
import Network.Socket.ByteString (recv)
import System.Timeout (timeout)

-- * EventlogNetworking

data EventlogNetworkingArgs = MkEventlogNetworkingArgs
  { socketPath :: FilePath
  }

data EventlogNetworking = MkEventlogNetworking
  { connInitTime  :: UTCTime
  , produceEvents :: IO [Event]
  , eventlogProducer :: Concurrently ()
  }

initEventlogProducer :: EventlogNetworkingArgs -> IO EventlogNetworking
initEventlogProducer MkEventlogNetworkingArgs{..} = do
  sock         <- initSocket socketPath
  connInitTime <- getCurrentTime
  eventsQueue  <- newTBQueueIO 1_000_000
  let
    produceEvents = atomically (flushTBQueue eventsQueue)
    eventlogProducer = Concurrently do
      streamFromSocket (atomically . writeTBQueue eventsQueue) sock
  pure MkEventlogNetworking{..}

initSocket :: String -> IO Socket
initSocket socketPath = do
  mSock <-
    timeout 3_000_000 (
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
  maybe (error "Socket connection timeout") pure mSock

streamFromSocket :: (Event -> IO ()) -> Socket -> IO ()
streamFromSocket writeEvent sock = do
  buffer <- initBuffer sock
  header <- readHeader buffer decodeHeader
  (void . forever) (processEvents buffer writeEvent (decodeEvents header))

processEvents :: Buffer -> (a -> IO ()) -> Decoder a -> IO ()
processEvents buff writeEvent decoder = case decoder of
  Produce res dec -> do
    writeEvent res
    processEvents buff writeEvent dec
  Consume dec -> do
    buffValue <- readBuffer buff
    processEvents buff writeEvent (dec buffValue)
  Error _bs err -> error err
  Done _ -> error "Unexpected done"


readHeader :: Buffer -> Decoder Header -> IO Header
readHeader buff decoder = case decoder of
  Consume dec -> readHeader buff . dec =<< readBuffer buff
  Produce res (Done left) -> writeToBuffer buff left *> pure res
  Error _bs err -> error err
  Produce _res _dec -> error "Unexpected extra result in header decoder"
  Done _ -> error "Unexpected done in header decoder"


data Buffer = MkBuffer {bufferSocket :: Socket, buff :: IORef ByteString}

initBuffer :: Socket -> IO Buffer
initBuffer sock = MkBuffer sock <$> newIORef ""

writeToBuffer :: Buffer -> ByteString -> IO ()
writeToBuffer MkBuffer{..} val = void (atomicModifyIORef buff (val,))

readBuffer :: Buffer -> IO ByteString
readBuffer MkBuffer{..} =
  readIORef buff
    >>= (\currentBuffer ->
      case BS.length currentBuffer of
        0 -> do
          received <- recv bufferSocket 4096
          if BS.null received
            then error "Socket closed"
            else pure received
        _ -> atomicWriteIORef buff "" *> pure currentBuffer
    )
