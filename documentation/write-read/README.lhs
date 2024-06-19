---
title: Write and read
---

# ClickHaskell: write and read example

An example of writing and reading data from a table in ClickHouse.
To try out this example in repl: setup [development environment](https://github.com/GetShopTV/ClickHaskell#development-environment) and then run

```bash
cabal run example-write-read
```

## Code Example

```haskell
{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeApplications
#-}

import ClickHaskell.Client (WritableInto, ReadableFrom, selectFrom, insertInto, ChCredential(..))
import ClickHaskell.Tables (Table, Column)
import ClickHaskell.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , LowCardinality, Nullable
  )
import Control.Concurrent.STM (newTQueueIO, atomically, writeTQueue)
import Data.ByteString (StrictByteString)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)


main :: IO ()
main = do
  let credentials = MkChCredential
        { chLogin = "default"
        , chPass = ""
        , chUrl = "http://localhost:8123"
        , chDatabase = "default"
        }

  manager <- newManager defaultManagerSettings

  queue <- newTQueueIO

  atomically (writeTQueue queue exampleDataSample)

  insertInto
    @ExampleTable
    @ExampleData
    manager
    credentials
    queue

  mapM_ print
    =<<
      selectFrom
        @ExampleTable
        @ExampleData
        manager
        credentials


type ExampleTable =
  Table
    "exampleWriteRead"
   '[ Column "a1" ChInt64
    , Column "a2" (LowCardinality ChString)
    , Column "a3" ChDateTime
    , Column "a4" ChUUID
    , Column "a5" ChInt32
    , Column "a6" (LowCardinality (Nullable ChString))
    , Column "a7" (LowCardinality ChString)
    ]

data ExampleData = MkExampleData
  { a1 :: ChInt64
  , a2 :: StrictByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: LowCardinality ChString
  }
  deriving (Generic, Show)

instance ReadableFrom ExampleTable ExampleData
instance WritableInto ExampleTable ExampleData

exampleDataSample :: ExampleData
exampleDataSample = MkExampleData
  { a1 = toChType (42 :: Int64)
  , a2 = "text"
  , a4 = toChType (0 :: Word64)
  , a3 = 42 
  , a5 = 42
  , a6 = Just "500"
  , a7 = ""
  }
```

## ClickHouse

ClickHouse script used for this example:

```sql
CREATE TABLE exampleWriteRead
(
    `a1` Int64,
    `a2` LowCardinality(String),
    `a3` DateTime,
    `a4` UUID,
    `a5` Int64,
    `a6` LowCardinality(Nullable(String)),
    `a7` LowCardinality(String)
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ()
```
