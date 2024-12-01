{-# LANGUAGE OverloadedStrings #-}

module IntegrationTests
  ( main
  ) where

-- Internal
import ClickHaskell (ChCredential(..), openNativeConnection)
import T1QuerySerialization (querySerializationTest)
import T2WriteReadEquality (writeReadEqualityTest)

main :: IO ()
main = do
  let credentials = MkChCredential
        { chLogin    = "default"
        , chPass     = ""
        , chHost     = "localhost"
        , chDatabase = "default"
        , chPort     = "9000"
        }
  connection <- openNativeConnection credentials

  querySerializationTest connection
  writeReadEqualityTest connection
