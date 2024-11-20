{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
#-}

module IntegrationTests
  ( main
  ) where

-- Internal
import ClickHaskell (ChCredential(..), openNativeConnection)
import ClickHaskell.DbTypes
import IntegrationTests.QuerySerialization (querySerializationTest)
import IntegrationTests.WriteReadEquality (runWriteReadEqualityTest)

-- GHC included
import Data.ByteString as BS (singleton)

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

  querySerializationTest @ChInt8 connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChInt16 connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChInt32 connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChInt64 connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChUInt8 connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChUInt16 connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChUInt32 connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChUInt64 connection [minBound, toEnum 0, maxBound]
  -- ToDo: querySerializationTest @ChUUID connection [minBound, toEnum 0, maxBound]
  querySerializationTest @ChString connection (map (toChType . BS.singleton) [1..255])
  -- ToDo: querySerializationTest @(LowCardinality ChString) connection (map (toChType . BS.singleton) [0..255])
  -- ToDo: querySerializationTest @(ChArray ChString) connection [toChType $ map BS.singleton [0..255]]
  -- ToDo: querySerializationTest @(ChArray ChInt64) connection [toChType [0 :: ChInt64 .. 255]]
  runWriteReadEqualityTest connection
