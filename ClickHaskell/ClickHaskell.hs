{-# LANGUAGE
    OverloadedStrings
#-}
module ClickHaskell
  ( module ClickHaskell.DbTypes
  , openNativeConnection
  , dev
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Native (openNativeConnection)
import ClickHaskell.Native.Packets
  ( mkHelloPacket
  , mkPingPacket
  , mkQueryPacket
  , mkDataPacket
  , ChCredential(..)
  )

-- GHC included
import Data.ByteString.Builder (toLazyByteString)

-- External
import Network.Socket.ByteString.Lazy (recv, sendAll)


dev :: IO ()
dev = do
  sock <- openNativeConnection devCredential

  (sendAll sock . toLazyByteString) (mkHelloPacket 54467 devCredential)
  (sendAll sock . toLazyByteString) "\0"
  print =<< recv sock 4096

  (sendAll sock . toLazyByteString) mkPingPacket
  print =<< recv sock 4096

  (sendAll sock . toLazyByteString)
    (  mkQueryPacket 54467 devCredential "SELECT 5"
    <> mkDataPacket "" False
    )
  print =<< recv sock 4096

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
