{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module ChEventlogWriter where

import ClickHaskell (Connection)
import Control.Exception (SomeException, bracketOnError, catch, finally)
import GHC.Eventlog.Socket (start)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import Network.Socket
import Network.Socket.ByteString (recv)
import System.Timeout (timeout)

chEventlogWrite :: IO Connection -> FilePath -> IO ()
chEventlogWrite _initConn socketPath = do
  start socketPath
  _sock <- 
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
  pure ()

streamFromSocket :: Socket -> IO ()
streamFromSocket sock = loop decodeEventLog
  where
  loop = \case
    Error   _bs err -> error err
    Produce _a _dec -> error "a"
    Consume dec     -> loop . dec =<< recv sock 4096
    Done _leftover  -> putStrLn "Done"
