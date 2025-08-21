{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module ClickHaskell.TLS where

-- Internal
import ClickHaskell (ConnectionArgs, Buffer(..), overrideNetwork, ConnectionError(..))

-- GHC included
import Control.Exception (SomeException, bracketOnError, catch, finally, throwIO)
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef (newIORef)
import System.Timeout (timeout)

-- External
import Network.Socket (AddrInfo (..), ShutdownCmd (..), SocketOption (..), close, connect, setSocketOption, shutdown, socket)
import Network.TLS (ClientParams (..), contextClose, contextNew, defaultParamsClient, handshake, recvData, sendData)

{-|
  Sets TLS connection

  Uses 9443 port by default. Watch 'setPort' to override it
-}
setSecure :: (ClientParams -> ClientParams) -> ConnectionArgs -> ConnectionArgs
setSecure modifyParams = overrideNetwork "9443" initTLS
  where
  initTLS = \hostname AddrInfo{..} -> do
    sock <- maybe (throwIO EstablishTimeout) pure
      =<< timeout 3_000_000 (
        bracketOnError
          (socket addrFamily addrSocketType addrProtocol)
          (\sock ->
            catch @SomeException
              (finally (shutdown sock ShutdownBoth) (close sock))
              (const $ pure ())
          )
          (\sock -> do
            setSocketOption sock NoDelay 1
            setSocketOption sock KeepAlive 1
            connect sock addrAddress
            pure sock
          )
        )
    let defClientParams = modifyParams (defaultParamsClient hostname "")
    context <- contextNew sock defClientParams
    handshake context
    buff <- newIORef ""
    pure
      MkBuffer
        { writeSock = \bs -> sendData context (toLazyByteString bs)
        , readSock  = recvData context
        , closeSock = contextClose context
        , buff
        }
