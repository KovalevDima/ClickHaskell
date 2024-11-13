---
title: Writing
---

Lets imagine we want build integration with table

```sql
CREATE TABLE exampleWriteRead
(
    `a1` Int64,
    `a2` LowCardinality(String),
    `a3` DateTime,
    `a4` UUID,
    `a5` Int32,
    `a6` LowCardinality(Nullable(String)),
    `a7` LowCardinality(String)
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
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- Describe your table

type ExampleTable =
  Table
    "exampleWriteRead"
   '[ Column "a1" ChInt64
    {- FIXME:
    , Column "a2" (LowCardinality ChString)
    , Column "a6" (LowCardinality (Nullable ChString))
    , Column "a7" (LowCardinality ChString)
    -}
    , Column "a3" ChDateTime
    , Column "a4" ChUUID
    , Column "a5" ChInt32
    ]

-- Define your model

data ExampleData = MkExampleData
  { a1 :: ChInt64
  {- FIXME:
  , a2 :: StrictByteString
  , a6 :: Nullable ChString
  , a7 :: LowCardinality ChString
  -}
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
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
        {- FIXME:
        , a2 = "text"
        , a6 = Just "500"
        , a7 = ""
        -}
        , a4 = toChType (0 :: Word64)
        , a3 = 42 
        , a5 = 42
      }
    ]

```
