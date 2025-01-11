<h1>selectFromView</h1>


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
  , DeriveAnyClass
  , DerivingStrategies
  , OverloadedStrings
#-}

module ReadingView where

import ClickHaskell
  ( ReadableFrom, selectFromView, Column
  , View, Parameter, parameter
  , openNativeConnection, defaultCredentials
  , ChString, ChInt32
  )
import Data.Int (Int32)
import GHC.Generics (Generic)

main :: IO ()
main = do
  connection <- openNativeConnection defaultCredentials
  mapM_ print
    =<<
      selectFromView
        @ExampleView
        @ExampleViewRecord
        connection
        ( parameter @"a1MoreThan" @ChInt32 ((-100_000) :: Int32)
        . parameter @"a1LessThan" @ChInt32 ((100_000) :: Int32)
        )

{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving anyclass instance ReadableFrom ExampleView ExampleViewRecord

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
```