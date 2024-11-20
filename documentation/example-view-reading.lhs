---
title: Reading from view
---

Lets imagine we have database with parametrized view `exampleParametrizedView` 
```sql
CREATE VIEW exampleParametrizedView
AS SELECT *
FROM generateRandom('a1 Int32, a2 Int32, a3 String', 1, 10, 2)
WHERE (a1 > {a1MoreThan:Int32}) AND (a1 < {a1LessThan:Int32})
LIMIT 5;
```

To perform a SELECT from such view you can use this snippet

```haskell
{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

import ClickHaskell
  ( ReadableFrom, selectFromView, Column
  , ChCredential(..)
  , View, Parameter, parameter
  , openNativeConnection
  )
import ClickHaskell.DbTypes (ChString, ChInt32)
import Data.Int (Int32)
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
  mapM_ print
    =<<
      selectFromView
        @ExampleView
        @ExampleViewRecord
        connection
        ( parameter @"a1MoreThan" @ChInt32 ((-100_000) :: Int32)
        . parameter @"a1LessThan" @ChInt32 ((100_000) :: Int32)
        )

type ExampleView =
  View
    "exampleParametrizedView"
   '[ Column "a1" ChInt32
    , Column "a2" ChInt32
    , Column "a3" ChString
    ]
   '[ Parameter "a1MoreThan" ChInt32
    , Parameter "a1LessThan" ChInt32
    ]

newtype ExampleViewRecord = MkExampleViewRecord
  { a1 :: Int32
  }
  deriving (Generic, Show)

instance ReadableFrom ExampleView ExampleViewRecord
```
