Lets write simple executable with basic usage example

Before we start we need to define module
```haskell
{-# LANGUAGE
  DataKinds,
  DeriveAnyClass,
  DeriveGeneric,
  OverloadedStrings,
  TypeApplications
#-}

module Main where

import ClickHaskell
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
```

ClickHaskell provides unique API in area of DBMS clients

We need to define our types and derive ClickHaskell instance

```haskell
type ExampleCols =
 '[ Column "a1" Int32
  , Column "a2" ChString
  , Column "a3" (DateTime "")
  ]

data ExampleData = MkExampleData
  { a2 :: ByteString
  , a3 :: UTCTime
  }
  deriving (Generic, ClickHaskell ExampleCols)
```

Define queries

```haskell
createTableQuery :: Command
createTableQuery  =
  "CREATE TABLE IF NOT EXISTS exampleTable \
  \ ( \
  \  `a1` Int32, \
  \  `a2` String, \
  \  `a3` DateTime, \
  \ ) \
  \ENGINE = MergeTree \
  \PARTITION BY () \
  \ORDER BY ();"

selectQuery :: Select ExampleCols ExampleData
selectQuery = fromGenerateRandom @ExampleCols @ExampleData (1, 10, 2) 1
  
insertQuery :: Insert ExampleCols ExampleData
insertQuery = intoTable @"exampleTable" @ExampleCols @ExampleData
```

## Create table, read and then write

```haskell
main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs

  command connection createTableQuery

  batches <- select selectQuery connection (\batch -> pure batch)

  insert insertQuery connection (mconcat batches)
```
