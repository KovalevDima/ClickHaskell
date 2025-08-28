{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module ClickHaskell.TLS where

-- Internal
import ClickHaskell (ConnectionArgs, overrideNetwork, mkBuffer)

-- GHC included
import Data.ByteString.Builder (toLazyByteString)

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
    mkBuffer
      context
      (\c bs -> sendData c (toLazyByteString bs))
      (recvData)
      (contextClose)
