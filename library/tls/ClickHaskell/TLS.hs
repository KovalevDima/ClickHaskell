module ClickHaskell.TLS where

-- Internal
import ClickHaskell
  ( ConnectionArgs(..)
  , Buffer(..)
  )

-- GHC included
import Data.IORef (newIORef)

-- External
import Network.TLS (ClientParams (..), contextNew, contextClose, sendData, recvData, defaultParamsClient, handshake)
import Network.Socket as Sock (connect, setSocketOption, SocketOption (..))

{-|
  Sets TLS connection

  Uses 9443 port by default. Watch 'setPort' to override it
-}
setSecure :: (ClientParams -> ClientParams) -> ConnectionArgs -> ConnectionArgs
setSecure modifyParams MkConnectionArgs{..} =
  MkConnectionArgs{initBuffer = initTLS, isTLS=True, ..}
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
        { writeSock = \bs -> sendData context bs
        , readSock  = recvData context
        , closeSock = contextClose context
        , buff
        }