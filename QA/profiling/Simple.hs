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

module Simple (main) where

-- Internal
import ClickHaskell
import ClickHaskell.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , Nullable
  )

-- GHC included
import Control.Concurrent (threadDelay)
import Data.ByteString (StrictByteString)
import Data.ByteString.Builder (string8)
import Data.Int (Int32)
import Data.Word (Word32)
import Debug.Trace (traceMarkerIO)
import GHC.Generics (Generic)


main :: IO ()
main = do
  traceMarkerIO "Initialization"  
  let credentials = MkChCredential "default" "" "" "localhost" "9000"
  connection <- openNativeConnection credentials

  let totalRows = 1_000_000 :: Integer

  threadDelay 250_000
  traceMarkerIO "Push data"

  traceMarkerIO "Starting reading"
  selectedData <-
    select
      @ExampleColumns
      @ExampleData
      connection
      (toChType $
        "SELECT * FROM generateRandom('\
        \a1 Int64, \
        \a2 String, \
        \a3 DateTime, \
        \a4 UUID, \
        \a5 Int32, \
        \a6 Nullable(String), \
        \a7 String\
        \', 1, 10, 2) LIMIT " <> (string8 . show) totalRows
      )

  threadDelay 1_000_000
  traceMarkerIO "Starting writing"
  insertInto
    @(Table "profiler" ExampleColumns)
    connection
    selectedData

  traceMarkerIO "Completion"
  print $ "Writing done. " <> show totalRows <> " rows was written"
  threadDelay 1_000_000


data ExampleData = MkExampleData
  { a1 :: ChInt64
  , a3 :: Word32
  , a4 :: ChUUID
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
 '[ Column "a1" ChInt64
  , Column "a2" ChString
  , Column "a3" ChDateTime
  , Column "a4" ChUUID
  , Column "a5" ChInt32
  , Column "a6" (Nullable ChString)
  , Column "a7" ChString
  ]
