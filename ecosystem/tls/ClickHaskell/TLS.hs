{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module ClickHaskell.TLS where

-- Internal
import ClickHaskell (ConnectionArgs, Buffer(..), overrideNetwork)

-- GHC included
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef (newIORef)

-- External
import Network.TLS (ClientParams (..), contextClose, contextNew, defaultParamsClient, handshake, recvData, sendData)

{-|
  Sets TLS connection

  Uses 9443 port by default. Watch 'setPort' to override it
-}
setSecure :: (ClientParams -> ClientParams) -> ConnectionArgs -> ConnectionArgs
setSecure modifyParams = overrideNetwork "9443" initTLS
  where
  initTLS = \hostname sock -> do
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
