{-# LANGUAGE
    OverloadedStrings
#-}
module ClickHaskell
  ( module ClickHaskell.DbTypes
  , openNativeConnection
  , ping
  , dev
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Native (openNativeConnection, ping)
import ClickHaskell.Native.Packets (ChCredential(..))

-- GHC included
import Control.Exception (throw)


dev :: IO ()
dev = do
  connection
    <- either throw pure
    =<< openNativeConnection devCredential
  ping connection

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
