
```haskell
{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeApplications
  , UndecidableInstances
  , ScopedTypeVariables
#-}

module Main (main) where

import ClickHaskell
import Control.Monad (when)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (byteString)
import GHC.Stack (HasCallStack)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  runTestForType @ChString connection "hello"
  runTestForType @(Array ChString) connection (toChType @_ @[ChString] [""])
  runTestForType @Bool connection True
  runTestForType @UInt8 connection 255
  runTestForType @(Array UInt8) connection (toChType @_ @[UInt8] [0,255])
  runTestForType @UInt16 connection 1000
  runTestForType @(Array UInt16) connection (toChType @_ @[UInt16] [0,255])
  runTestForType @UInt32 connection 1000
  runTestForType @(Array UInt32) connection (toChType @_ @[UInt32] [0,255])
  runTestForType @UInt64 connection 1000
  runTestForType @(Array UInt64) connection (toChType @_ @[UInt64] [0,255])
  runTestForType @UInt128 connection 1000
  runTestForType @(Array UInt128) connection (toChType @_ @[UInt128] [0,255])
  runTestForType @UInt256 connection 1000
  runTestForType @(Array UInt256) connection (toChType @_ @[UInt256] [0,255])
  runTestForType @Int8 connection (-127)
  runTestForType @(Array Int8) connection (toChType @_ @[Int8] [0, -128])
  runTestForType @Int16 connection (-1000)
  runTestForType @(Array Int16) connection (toChType @_ @[Int16] [0, -255])
  runTestForType @Int32 connection (-1000)
  runTestForType @(Array Int32) connection (toChType @_ @[Int32] [0, -255])
  runTestForType @Int64 connection (-1000)
  runTestForType @(Array Int64) connection (toChType @_ @[Int64] [0, -255])
  runTestForType @Float connection 10000000.0001
  runTestForType @(Array Float) connection (toChType @_ @[Float] [0, -10000000.0001])
  runTestForType @Double connection 10000000.0001
  runTestForType @(Array Double) connection (toChType @_ @[Double] [0, -10000000.0001])

  putStrLn "Ok"


runTestForType ::
  forall chType
  .
  ( ClickHaskell '[Column "testSample" chType] (TestSample chType)
  , IsParameterType chType
  , Show chType
  , Eq chType
  , IsChType chType
  , HasCallStack
  )
  =>
  Connection -> chType -> IO ()
runTestForType conn chType = do
  let typeName = (byteString . BS8.pack) (chTypeName @chType)
      query :: Select '[Column "testSample" chType] (TestSample chType)
      query =
        passParameters
          ( id
            . addParameter @chType "param" chType
          )
          (unsafeMkSelect (\_ -> "SELECT {param:" <> typeName <> "} as testSample"))

  [MkTestSample result] <- mconcat <$> select query conn pure

  when (result /= chType) (
    error (
      "Deserialized value of type " <> show typeName <> " unmatched:"
      <> " Expected: " <> show chType
      <> ". But got: " <> show result <> "."
      )
    ) 
  pure ()


data TestSample chType = MkTestSample
  { testSample :: chType
  }
  deriving (Generic, Show)


instance
  ( SerializableColumn (Column "testSample" chType)
  , KnownColumn (Column "testSample" chType)
  )
  =>
  ClickHaskell '[Column "testSample" chType] (TestSample chType)

```
