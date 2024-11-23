---
title: Writing
---

Lets imagine we want build integration with table

```sql
CREATE TABLE exampleWriteRead
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

You can follow next steps:

```haskell
-- Prepare module

{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , OverloadedStrings
#-}

module Writing where

import ClickHaskell
  ( WritableInto, insertInto
  , ChCredential(..), openNativeConnection
  , Table, Column
  )
import ClickHaskell.DbTypes
import Data.ByteString
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- Describe your table

type ExampleTable =
  Table
    "exampleWriteRead"
   '[ Column "a1" ChInt64
    , Column "a2" ChString
    , Column "a3" ChDateTime
    , Column "a4" ChUUID
    , Column "a5" ChInt32
    , Column "a6" (Nullable ChString)
    , Column "a7" ChString
    ]

-- Define your model

data ExampleData = MkExampleData
  { a1 :: ChInt64
  , a2 :: ByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: LowCardinality ChString
  }
  deriving (Generic, Show)

-- Derive integration

deriving instance WritableInto ExampleTable ExampleData

-- Write an integration

main :: IO ()
main = do
  let credentials = MkChCredential
        { chLogin = "default"
        , chPass = ""
        , chHost = "localhost"
        , chDatabase = "default"
        , chPort = "9000"
        }
  connection <- openNativeConnection credentials 

  insertInto
    @ExampleTable
    @ExampleData
    connection
    [ MkExampleData
        { a1 = toChType (42 :: Int64)
        , a2 = "text"
        , a4 = toChType (0 :: Word64)
        , a3 = 42 
        , a5 = 42
        , a6 = Just "500"
        , a7 = ""
      }
    ]
```
