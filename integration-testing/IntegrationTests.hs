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
import IntegrationTests.Serialization (runSerializationTest)
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
  
  runSerializationTest @ChInt32 connection [minBound, toEnum 0, maxBound]
  runSerializationTest @ChInt64 connection [minBound, toEnum 0, maxBound]
  runSerializationTest @ChUInt32 connection [minBound, toEnum 0, maxBound]
  runSerializationTest @ChUInt64 connection [minBound, toEnum 0, maxBound]
  runSerializationTest @ChString connection (map (toChType . BS.singleton) [1..255])
  -- runSerializationTest @(ChArray ChString) connection [toChType $ map BS.singleton [0..255]]
  -- runSerializationTest @(ChArray ChInt64) connection [toChType [0 :: ChInt64 .. 255]]
