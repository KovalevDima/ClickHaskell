{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module ClickHouseProxy.TcpServer (
  runTcpServer
) where

import ClickHaskell (ToChType (..))
import ClickHaskell.Protocol (ClientPacket (..), ExceptionPacket (..), HelloPacket (..), Serializable (..), ServerPacket (..))
import Control.Concurrent (forkFinally)
import Control.Exception as E (SomeException, bracket, bracketOnError, catch, finally)
import Control.Monad (forever, void)
import Data.Binary (Get)
import Data.Binary.Get (Decoder (..), runGetIncremental)
import Data.ByteString as BS (ByteString, null, toStrict)
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS (putStrLn)
import Data.ByteString.Char8 as BS8 (pack)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty as NE (head)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)


runTcpServer :: IO ()
runTcpServer =
  runTCPServer Nothing "9000" \sock -> do
    BS.putStrLn "Opening connection"
    buffer <- mkBuffer sock
    talk buffer
  where
  talk buffer = do
    packet <- readBuffer buffer (deserialize @ClientPacket 54459)
    case packet of
      Hello hello -> do
        Prelude.putStrLn $
          "client: "               <> show hello.client_name          <> " " <>
          "client_version_major: " <> show hello.client_version_major <> " " <>
          "client_version_minor: " <> show hello.client_version_minor <> " " <>
          "tcp_protocol_version: " <> "54459"                         <> " " <>
          "default_database: "     <> show hello.default_database     <> " " <>
          "user: "                 <> show hello.user                 <> " " <>
          "pass: "                 <> show hello.pass                 <> " " <>
          ""
        writeConn buffer
          . serialize 54459
          $ Exception MkExceptionPacket{
              code        = 0,
              name        = toChType @_ @ByteString $ BS8.pack "Test exception",
              message     = toChType @_ @ByteString $ BS8.pack "\"Hello, World!\"",
              stack_trace = toChType @_ @ByteString $ BS8.pack "",
              nested      = 0
            }
      _other -> error "Unexpected packet"


runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = do
    addr <- resolve
    E.bracket (open addr) close loop
  where
  resolve = do
    let hints = defaultHints {
          addrFlags = [AI_PASSIVE],
          addrSocketType = Stream
        }
    NE.head <$> getAddrInfo (Just hints) mhost (Just port)
  open addr =
    E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
  loop sock =
    forever $ E.bracketOnError (accept sock) (close . fst)
      $ \(conn, _peer) -> void $
          forkFinally (server conn) (const $ gracefulClose conn 5000)


-- Buffer

mkBuffer :: Socket -> IO Buffer
mkBuffer sock = do
  buff <- newIORef mempty
  let
    writeBuff = writeIORef buff
    writeConn = sendAll sock . toStrict . toLazyByteString
    readBuff = do
      currentBuffer <- readIORef buff
      if (not . BS.null) currentBuffer
      then writeBuff mempty *> pure currentBuffer
      else do
        sockBytes <- recv sock 4096
        if BS.null sockBytes
        then error "Connection closed"
        else pure sockBytes
    destroyBuff = do
      catch @SomeException
        (finally (shutdown sock ShutdownBoth) (close sock))
        (const $ pure ())
      writeBuff mempty
  pure MkBuffer{writeConn, writeBuff, readBuff, destroyBuff}
  where
  

data Buffer = MkBuffer
  { readBuff :: IO ByteString
  , destroyBuff :: IO ()
  , writeBuff :: ByteString -> IO ()
  , writeConn :: Builder -> IO ()
  }

readBuffer :: Buffer -> Get a -> IO a
readBuffer MkBuffer{readBuff, writeBuff} parser = runBufferReader (runGetIncremental parser)
  where
  runBufferReader :: Decoder packet -> IO packet
  runBufferReader dec = case dec of
    (Partial decoder) -> readBuff >>= runBufferReader . decoder . Just
    (Done leftover _consumed packet) -> packet <$ writeBuff leftover
    (Fail _leftover _consumed msg) -> error ("Deserialization error: " <> msg)
