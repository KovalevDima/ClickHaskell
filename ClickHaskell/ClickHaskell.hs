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
import ClickHaskell.Native (openNativeConnection, ping, selectFrom)
import ClickHaskell.Native.Packets (ChCredential(..))

-- GHC included
import Control.Monad (replicateM_)


dev :: IO ()
dev = do
  connection <- openNativeConnection devCredential
  replicateM_ 5000 (ping connection)
  replicateM_ 50000 (selectFrom connection)

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
