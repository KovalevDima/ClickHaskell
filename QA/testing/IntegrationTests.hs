module IntegrationTests (main) where

import ClickHaskell (ChCredential(..), openNativeConnection, defaultCredentials)
import T1QuerySerialization (querySerializationTest)
import T2WriteReadEquality (writeReadEqualityTest)

main :: IO ()
main = do
  connection <- openNativeConnection defaultCredentials

  querySerializationTest connection
  writeReadEqualityTest connection
