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
import ClickHaskell.Native.Versioning
  ( afterRevision
  , DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM
  , latestSupportedRevision
  )

-- GHC included
import Data.ByteString.Builder (toLazyByteString)

-- External
import Network.Socket.ByteString.Lazy (recv, sendAll)


dev :: IO ()
dev = do
  sock <- openNativeConnection devCredential

  (sendAll sock . toLazyByteString) (mkHelloPacket latestSupportedRevision devCredential)
  afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM latestSupportedRevision
    (sendAll sock . toLazyByteString) "\0"
  print =<< recv sock 4096

  (sendAll sock . toLazyByteString) mkPingPacket
  print =<< recv sock 4096

  (sendAll sock . toLazyByteString)
    (  mkQueryPacket latestSupportedRevision devCredential "SELECT 5"
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
