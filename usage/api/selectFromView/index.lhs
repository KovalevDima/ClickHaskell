<h1>selectFromView</h1>


Lets imagine we have database with parametrized view <b>exampleParametrizedView</b>

<pre><code class="sql" data-lang="sql"
>CREATE VIEW exampleParametrizedView
AS SELECT *
FROM generateRandom('a1 Int32, a2 Int32, a3 String', 1, 10, 2)
WHERE (a1 > {a1MoreThan:Int32}) AND (a1 < {a1LessThan:Int32})
LIMIT 5;
</code></pre>

To perform a <b>SELECT</b> from such view you can use this snippet

<pre><code data-lang="haskell" class="haskell"
>{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DerivingStrategies
  , OverloadedStrings
#-}

module Main where

import ClickHaskell
  ( ReadableFrom, selectFromView, Column
  , View, Parameter, parameter
  , openConnection, defaultConnectionArgs
  , ChString, Int32
  )
import GHC.Generics (Generic)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  _ <-
    selectFromView
      @ExampleView
      @ExampleViewRecord
      connection
      ( parameter @"a1MoreThan" @Int32 ((-100_000) :: Int32)
      . parameter @"a1LessThan" @Int32 ((100_000) :: Int32)
      )
      print
  pure ()

{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving anyclass instance ReadableFrom ExampleView ExampleViewRecord

type ExampleView =
  View
    "exampleParametrizedView"
   '[ Column "a1" Int32
    , Column "a2" Int32
    , Column "a3" ChString
    ]
   '[ Parameter "a1MoreThan" Int32
    , Parameter "a1LessThan" Int32
    ]

newtype ExampleViewRecord = MkExampleViewRecord
  { a1 :: Int32
  }
  deriving (Generic, Show)
</code></pre>
