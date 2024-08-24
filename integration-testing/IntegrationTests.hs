{-# LANGUAGE
  OverloadedStrings
#-}

module IntegrationTests
  ( main
  ) where

import ClickHaskell.Client (ChCredential(..))
import IntegrationTests.Serialization (runSerializationTests)
import IntegrationTests.WriteReadEquality (runWriteReadEqualityTest)

main :: IO ()
main = do
  let cred =
        MkChCredential
          { chLogin="default"
          , chPass=""
          , chUrl="http://localhost:8123"
          , chDatabase="default"
          }
  runSerializationTests cred
  runWriteReadEqualityTest cred
