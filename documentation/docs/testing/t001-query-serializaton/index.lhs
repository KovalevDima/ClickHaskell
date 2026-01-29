# Query serialization test

- Builds queries like
  ```sql
  SELECT CAST(5, 'UInt8') as testSample;
  ```
  via <b>ToQueryPart</b> type class for every supported type
- Executes *select*
- Parses the result
- Checks if result equals initial value

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

-- Internal
import ClickHaskell

-- GHC included
import Control.Monad (unless, forM_)
import Data.ByteString as BS (singleton, ByteString)
import Data.ByteString.Char8 as BS8 (pack)
import Data.ByteString.Builder (byteString)
import GHC.Generics (Generic)


main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  connOld <- openConnection (overrideMaxRevision 1 defaultConnectionArgs)

  runQuerySerialization connection
  runQuerySerialization connOld


runQuerySerialization :: Connection -> IO ()
runQuerySerialization conn = do
  runTestForType @Int8 conn [minBound, toEnum 0, maxBound]
  runTestForType @Int16 conn [minBound, toEnum 0, maxBound]
  runTestForType @Int32 conn [minBound, toEnum 0, maxBound]
  runTestForType @Int64 conn [minBound, toEnum 0, maxBound]
  runTestForType @Int128 conn [minBound, 0, maxBound]
  runTestForType @Int256 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt8 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt16 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt32 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt64 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt128 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt256 conn [minBound, toEnum 0, maxBound]
  runTestForType @UUID conn [minBound, toEnum 0, maxBound]
  runTestForType @(DateTime "") conn [minBound, toEnum 0, maxBound]
  runTestForType @(DateTime "Europe/Amsterdam") conn [minBound, toEnum 0, maxBound]
  runTestForType @Bool conn [False, True]
  runTestForType @(Enum8 "'hello' = 1") conn [minBound, toEnum 0, maxBound]
  runTestForType @(Enum16 "'hello' = 1") conn [minBound, toEnum 0, maxBound]
  -- unsupported: runTestForType @(DateTime64 0 "") conn [minBound, toEnum 0, maxBound]
  runTestForType @ChString conn (map (toChType . BS.singleton) [1..255])
  -- unsupported: runTestForType @(LowCardinality ChString) connection (map (toChType . BS.singleton) [0..255])
  runTestForType @(Array ChString) conn [toChType $ map BS.singleton [0..255]]
  runTestForType @(Array Int64) conn [toChType @(Array Int64) @[Int64] [0 .. 255]]
  runTestForTypeWith @Float32 conn cmpFloatSemantic [0, nan, negInf, posInf]
  runTestForTypeWith @Float64 conn cmpFloatSemantic [0, nan, negInf, posInf]

cmpFloatSemantic :: (Eq f, RealFloat f) => f -> f -> Bool
cmpFloatSemantic float1 float2
  | isNaN float1 && isNaN float2 = True
  | isInfinite float1 && isInfinite float2 = True
  | otherwise = float1 == float2

nan :: (Read a, Floating a) => a
nan = read "NaN"

negInf :: (Read a, Floating a) => a
negInf = read "-Infinity"

posInf :: (Read a, Floating a) => a
posInf = read "-Infinity"

runTestForType ::
  ( ToQueryPart chType
  , IsChType chType
  , Eq chType
  , Show chType
  , ClickHaskell '[Column "testSample" chType] (TestSample chType)
  )
  =>
  Connection -> [chType] -> IO ()
runTestForType connection testValues = runTestForTypeWith connection (==) testValues

runTestForTypeWith ::
  forall chType
  .
  ( ToQueryPart chType
  , IsChType chType
  , Show chType
  , ClickHaskell '[Column "testSample" chType] (TestSample chType)
  )
  =>
  Connection -> (chType -> chType -> Bool) -> [chType] -> IO ()
runTestForTypeWith connection iqEqual testValues = do
  let typeName = (toChType @ChString . byteString . BS8.pack) (chTypeName @chType)
  forM_
    testValues
    (\chType -> do
      [selectChType] <-
        concat <$>
          select
            (unsafeMkSelect
              @'[Column "testSample" chType]
              @(TestSample chType)
              (\_cols -> "SELECT CAST(" <> toQueryPart chType <> ", " <> toQueryPart typeName <> ") as testSample;")
            )
            connection
            pure

      (unless (iqEqual chType (testSample selectChType)) . error)
        (  "Deserialized value of type " <> show typeName <> " unmatched:"
        <> " Expected: " <> show chType
        <> ". But got: " <> show selectChType <> "."
        )
    )

  print (fromChType @ChString @ByteString typeName <> ": Ok")


data TestSample chType = MkTestSample {testSample :: chType}
  deriving (Generic, Show)


instance
  ( SerializableColumn (Column "testSample" chType)
  , KnownColumn (Column "testSample" chType)
  )
  =>
  ClickHaskell '[Column "testSample" chType] (TestSample chType)

```
