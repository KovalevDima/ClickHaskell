<h1>insertInto</h1>

Lets imagine we want insert data into table

```sql
CREATE TABLE exampleWriteRead
(
    `a1` Int64,
    `a2` String,
    `a3` DateTime,
    `a4` UUID,
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ();
```

There are a simple "How to do" example:

```haskell
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
  , toChType
  , ChDateTime, ChInt64, ChString, ChUUID
  )
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

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
        { a1 = 42
        , a2 = "text"
        , a4 = toChType (0 :: Word64)
        , a3 = 42
        }
    ]

{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving instance WritableInto ExampleTable ExampleData

type ExampleTable =
  Table
    "exampleWriteRead"
   '[ Column "a1" ChInt64
    , Column "a2" ChString
    , Column "a3" ChDateTime
    , Column "a4" ChUUID
    ]

data ExampleData = MkExampleData
  { a1 :: Int64
  , a2 :: ByteString
  , a4 :: ChUUID
  , a3 :: Word32
  }
  deriving (Generic, Show)
```
