<h1>selectFromView</h1>

The `select` is a wrapper for generic select queries. Such as


```sql
SELECT CAST(5, 'UInt8') as num LIMIT 5;
```


```haskell
{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DerivingStrategies
  , OverloadedStrings
#-}

module Main where

import ClickHaskell
  ( ReadableFrom, select
  , Column, Columns
  , openNativeConnection, defaultCredentials
  , UInt8
  )
import GHC.Generics (Generic)

main :: IO ()
main = do
  connection <- openNativeConnection defaultCredentials
  _ <-
    select
      @ExampleColumns
      @ExampleTableRecord
      connection
      "SELECT CAST(5, 'UInt8') as num LIMIT 5;"
      print
  pure ()

{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving anyclass instance ReadableFrom (Columns ExampleColumns) ExampleTableRecord

type ExampleColumns =
   '[ Column "num" UInt8
    ]

newtype ExampleTableRecord = MkExampleTableRecord
  { num :: UInt8
  }
  deriving (Generic, Show)
```
