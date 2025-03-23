{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

{-
```sql
CREATE TABLE profiler
(
    `a1` Int64,
    `a2` String,
    `a3` DateTime,
    `a4` UUID,
    `a5` Int32,
    `a6` Nullable(String),
    `a7` String
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ();
```
-}

module Main (main) where

-- Internal
import ClickHaskell

-- GHC included
import Data.ByteString (StrictByteString)
import Data.Word (Word32)
import Debug.Trace (traceMarkerIO)
import GHC.Generics (Generic)


main :: IO ()
main = do
  traceMarkerIO "Initialization"  
  let credentials = MkChCredential "default" "" "" "localhost" "9000"
  readingConnection <- openNativeConnection credentials
  writingConnection <- openNativeConnection credentials

  let totalRows = 1_000_000

  _ <-
    generateRandom
      @ExampleColumns
      @ExampleData
      readingConnection
      (1, 10, 2)
      totalRows
      (insertInto @(Table "profiler" ExampleColumns) writingConnection)

  print $ "Writing done. " <> show totalRows <> " rows was written"


data ExampleData = MkExampleData
  { a1 :: Int64
  , a3 :: Word32
  , a4 :: UUID
  , a2 :: StrictByteString
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: ChString
  }
  deriving (Generic, Show)
  deriving anyclass
    ( ReadableFrom (Columns ExampleColumns)
    , WritableInto (Table "profiler" ExampleColumns)
    )


type ExampleColumns =
 '[ Column "a1" Int64
  , Column "a2" ChString
  , Column "a3" (DateTime "")
  , Column "a4" UUID
  , Column "a5" Int32
  , Column "a6" (Nullable ChString)
  , Column "a7" ChString
  ]
