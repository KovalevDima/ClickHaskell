---
title: Parametrized view
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

import ClickHaskell.Client (ReadableFrom, ChCredential(..), selectFromTableFunction)
import ClickHaskell.Tables (interpretTable, mkParameter, View, Parameter, Column)
import ClickHaskell.DbTypes (ChInt32, ChString)
import Data.Int (Int32)
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)

main :: IO ()
main = do
  let credential = MkChCredential
        { chLogin = "default"
        , chPass = ""
        , chUrl = "http://localhost:8123"
        , chDatabase = "default"
        }
  manager <- newManager defaultManagerSettings
  mapM_ print
    =<<
      selectFromTableFunction
        @ExampleView
        @ExampleViewRecord
        manager
        credential
        (interpretTable @ExampleView (mkParameter (-100_000)) (mkParameter 100_000))

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

To try out this example in repl: setup [development environment](https://github.com/GetShopTV/ClickHaskell#development-environment) and then run

```bash
cabal run example-parametrized-view

# output
MkExampleViewRecord {a1 = -23652}
MkExampleViewRecord {a1 = -82405}
MkExampleViewRecord {a1 = 92847}
MkExampleViewRecord {a1 = 6575}
MkExampleViewRecord {a1 = -80663}
```
