Lets write simple executable with basic usage example

Before we start we need to define module
```haskell
{-# LANGUAGE
  DataKinds,
  DeriveAnyClass,
  DeriveGeneric,
  OverloadedStrings,
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeApplications,
  StandaloneDeriving
#-}

module Main where

import ClickHaskell
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
  { a1 :: Int32
  , a2 :: ChString
  , a3 :: UInt32
  }
  deriving (Generic, ClickHaskell ExampleCols)
```

Define queries

```haskell
createTableQuery :: Command
createTableQuery  =
  "CREATE TABLE IF NOT EXISTS exampleTable ( \
  \  `a1` Int32, \
  \  `a2` String, \
  \  `a3` DateTime, \
  \) \
  \ENGINE = MergeTree \
  \PARTITION BY () \
  \ORDER BY ();"

selectQuery :: Select ExampleCols ExampleData
selectQuery =
  unsafeMkSelect @ExampleCols @ExampleData
    (\_cols ->
        "SELECT \
        \  defaultValueOfTypeName('Int32') as a1,   \
        \  defaultValueOfTypeName('String') as a2,  \
        \  defaultValueOfTypeName('DateTime') as a3 \
        \ "
    )

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
