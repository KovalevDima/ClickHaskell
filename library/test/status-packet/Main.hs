module Main (main) where

import System.Timeout (timeout)

import ClickHaskell (ClickHaskell, Column, UInt8, command, insert, ping, unsafeMkInsert)
import ClickHaskell.Connection
import ClickHaskell.Primitive (Serializable(..), Revisioned(..))
import ClickHaskell.Protocol.Data (mkDataPacket)
import ClickHaskell.Protocol.Server
import Control.Concurrent (newMVar)
import Control.Exception (SomeException, try)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import GHC.Generics (Generic)

main :: IO ()
main = do
  result <- timeout 5000000 runTests
  case result of
    Just () -> pure ()
    Nothing -> fail "regression suite timed out"

runTests :: IO ()
runTests = do
  conn <- scriptedConnection (statusPackets <> wire EndOfStream <> wire Pong)
  command conn "CREATE TABLE test ENGINE=Memory AS SELECT number FROM numbers(10)"
  ping conn
  inserted <- scriptedConnection (statusPackets <> wire (DataResponse (mkDataPacket "" 0 0))
    <> statusPackets <> wire EndOfStream <> wire Pong)
  insert (unsafeMkInsert @'[Column "value" UInt8] @Row (const "INSERT INTO test VALUES")) inserted [Row 1]
  ping inserted
  -- Reject actual data rows: status packets must not turn command into select.
  bad <- scriptedConnection (statusPackets <> wire (DataResponse (mkDataPacket "" 0 0)))
  rejected <- try @SomeException (command bad "SELECT 1")
  case rejected of
    Left _ -> pure ()
    _ -> fail "command accepted a data packet"
  exceptional <- scriptedConnection (statusPackets <> wire (UnknownPacket 42))
  failed <- try @SomeException (command exceptional "CREATE TABLE test (value UInt8)")
  case failed of
    Left _ -> pure ()
    _ -> fail "command accepted an unexpected packet"
  putStrLn "Command/insert status-packet regressions passed"

data Row = Row { value :: UInt8 }
  deriving stock (Generic)
  deriving anyclass (ClickHaskell '[Column "value" UInt8])

wire :: ServerPacket -> BS.ByteString
wire = BL.toStrict . toLazyByteString . serialize 1

statusPackets :: BS.ByteString
statusPackets =
  wire (Progress (MkProgressPacket 1 2 3 (BeforeRevision ()) (BeforeRevision ()) (BeforeRevision ()) (BeforeRevision ())))
  <> wire (ProfileInfo (MkProfileInfo 1 1 2 0 0 0 (BeforeRevision ()) (BeforeRevision ())))
  <> wire (ProfileEvents (MkProfileEventsPacket (mkDataPacket "" 6 0) [] [] [] [] [] []))

scriptedConnection :: BS.ByteString -> IO Connection
scriptedConnection response = do
  bytes <- newIORef response
  let readBuff = atomicModifyIORef' bytes (\bs -> (BS.empty, bs))
      writeBuff = writeIORef bytes
      buffer = MkBuffer { readBuff, writeBuff, destroyBuff = pure (), writeConn = const (pure ()) }
      state = MkConnectionState { buffer, revision = 1, initial_user = "", os_user = "", hostname = "", creds = defaultConnectionArgs { resolveHostName = \_ _ -> fail "test transport has no reconnect" } }
  MkConnection <$> newMVar state
