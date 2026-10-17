module Main (main) where

import ClickHaskell
import ClickHaskell.Connection (ConnectionArgs(..))
import Control.Concurrent (newEmptyMVar, takeMVar)
import Control.Exception (IOException, SomeException, bracket, throwIO, try)
import Control.Monad (replicateM_, unless)
import qualified Data.ByteString as BS
import Data.IORef
import Network.Socket
import System.Timeout (timeout)

main :: IO ()
main = do
  result <- timeout 5000000 runTests
  case result of
    Just () -> pure ()
    Nothing -> fail "regression suite timed out"

runTests :: IO ()
runTests = do
  withTransport $ \args closed readAction _ -> do
    writeIORef readAction (pure hello)
    conn <- openConnection args
    closeConnection conn
    closeConnection conn
    assertCount "idempotent close" closed 1
    expectClosed conn
  withTransport $ \args closed readAction _ -> do
    writeIORef readAction (pure hello)
    conn <- openConnection args
    writeIORef readAction (throwIO (userError "read failed"))
    failed <- try @IOException (ping conn)
    unless (either (const True) (const False) failed) (fail "operation did not fail")
    assertCount "failed operation closes transport" closed 1
    expectClosed conn
    closeConnection conn
    assertCount "failed handle does not close twice" closed 1
  replicateM_ 4 $ withTransport $ \args closed readAction _ -> do
    writeIORef readAction (pure hello)
    conn <- openConnection args
    blocked <- newEmptyMVar
    writeIORef readAction (takeMVar blocked)
    result <- timeout 100000 (ping conn)
    unless (result == Nothing) (fail "stalled operation did not time out")
    assertCount "cancelled operation closes transport" closed 1
    expectClosed conn
  withTransport $ \args closed readAction _ -> do
    writeIORef readAction (throwIO (userError "authentication failed"))
    expectOpenFailure args
    assertCount "failed authentication closes transport" closed 1
  withTransport $ \args closed readAction _ -> do
    blocked <- newEmptyMVar
    writeIORef readAction (takeMVar blocked)
    result <- timeout 100000 (openConnection args)
    case result of
      Nothing -> pure ()
      Just conn -> closeConnection conn >> fail "authentication did not time out"
    assertCount "cancelled authentication closes transport" closed 1
  withTransport $ \args _ _ sockets -> do
    let broken = args { mkBufferArgs = \_ _ -> throwIO (userError "transport init failed") }
    expectOpenFailure broken
    allocated <- readIORef sockets
    case allocated of
      [sock] -> do
        result <- try @IOException (getSocketName sock)
        unless (either (const True) (const False) result) (fail "initialization leaked its socket")
      _ -> fail "unexpected reconnection during initialization"
  -- Even a custom transport whose close throws must leave a closed handle.
  withTransport $ \args closed readAction _ -> do
    writeIORef readAction (pure hello)
    let broken = args { mkBufferArgs = \host sock -> do
          bufferArgs <- mkBufferArgs args host sock
          pure bufferArgs { closeSock = closeSock bufferArgs >> throwIO (userError "close failed") } }
    conn <- openConnection broken
    _ <- try @IOException (closeConnection conn)
    expectClosed conn
    closeConnection conn
    assertCount "throwing close invalidates handle" closed 1
  putStrLn "Connection lifecycle regressions passed"

-- Revision 1 HelloResponse: no optional handshake fields.
hello :: BS.ByteString
hello = BS.pack [0, 4, 116, 101, 115, 116, 1, 1, 1]

expectClosed :: Connection -> IO ()
expectClosed conn = do
  result <- timeout 100000 (try @ConnectionError (ping conn))
  case result of
    Just (Left ConnectionClosed) -> pure ()
    _ -> fail "failed/closed handle blocked or was reused"

expectOpenFailure :: ConnectionArgs -> IO ()
expectOpenFailure args = do
  result <- try @SomeException (openConnection args)
  case result of
    Left _ -> pure ()
    Right conn -> closeConnection conn >> fail "connection unexpectedly opened"

assertCount :: String -> IORef Int -> Int -> IO ()
assertCount label ref expected = do
  actual <- readIORef ref
  unless (actual == expected) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

-- The real socket is owned by the driver. Reads/writes use a scripted transport,
-- so these failure and cancellation tests do not need a ClickHouse server.
withTransport :: (ConnectionArgs -> IORef Int -> IORef (IO BS.ByteString) -> IORef [Socket] -> IO a) -> IO a
withTransport action = bracket (newIORef []) cleanup $ \sockets -> do
  closed <- newIORef 0
  readAction <- newIORef (pure hello)
  let args = (overrideMaxRevision 1 defaultConnectionArgs)
        { resolveHostName = \_ _ -> pure defaultHints
        , initSocket = \_ -> do
            sock <- socket AF_INET Stream defaultProtocol
            modifyIORef' sockets (sock :)
            pure sock
        , mkBufferArgs = \_ sock -> pure MkBufferArgs
            { writeSock = const (pure ())
            , readSock = readIORef readAction >>= id
            , closeSock = modifyIORef' closed (+1) >> close sock
            }
        }
  action args closed readAction sockets
  where
    cleanup sockets = readIORef sockets >>= mapM_ close
