# Thread safety test

Runs 10000 concurrent queries via single connection

```haskell
{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , TypeApplications
#-}

module Main (main) where

-- Internal
import ClickHaskell

-- GHC included
import GHC.Generics (Generic)
import Control.Concurrent.Async (replicateConcurrently_)


main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  connOld <- openConnection (overrideMaxRevision 1 defaultConnectionArgs)

  runMultithreading connection
  runMultithreading connOld


runMultithreading :: Connection -> IO ()
runMultithreading connection = do
  replicateConcurrently_ 10000 (
    select
      (fromGenerateRandom
        @ExampleColumns
        @ExampleData
        (1, 10, 2)
        1
      )
      connection
      pure
    )

data ExampleData = MkExampleData
  { a1 :: Int64
  }
  deriving (Generic)
  deriving anyclass (ClickHaskell ExampleColumns)


type ExampleColumns =
 '[ Column "a1" Int64
  ]
```
