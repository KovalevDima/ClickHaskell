module ClickHaskell.TLS where

-- Internal
import ClickHaskell (ConnectionArgs, Buffer(..), overrideNetwork)

-- GHC included
import Data.IORef (newIORef)
import Data.ByteString.Builder (toLazyByteString)

-- External
import Network.TLS (ClientParams (..), contextNew, contextClose, sendData, recvData, defaultParamsClient, handshake)
import Network.Socket as Sock (connect, setSocketOption, SocketOption (..))

{-|
  Sets TLS connection

  Uses 9443 port by default. Watch 'setPort' to override it
-}
setSecure :: (ClientParams -> ClientParams) -> ConnectionArgs -> ConnectionArgs
setSecure modifyParams conn = overrideNetwork True initTLS conn
  where
  initTLS = \hostname addrAddress sock -> do
    setSocketOption sock Sock.NoDelay 1
    setSocketOption sock Sock.KeepAlive 1
    connect sock addrAddress
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
