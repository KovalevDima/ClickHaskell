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
import ClickHaskell.NativeProtocol (ChCredential(..), openNativeConnection, ping, selectFrom)
import ClickHaskell.Tables ()

-- GHC included
import Control.Monad (replicateM_)


dev :: IO ()
dev = do
  connection <- openNativeConnection devCredential
  replicateM_ 500 (ping connection)
  replicateM_ 500 (selectFrom connection)

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
