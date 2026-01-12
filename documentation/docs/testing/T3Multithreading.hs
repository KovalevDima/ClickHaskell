{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , TypeApplications
#-}

module T3Multithreading where

-- Internal
import ClickHaskell

-- GHC included
import Control.Concurrent.Async (replicateConcurrently_)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)


t3 :: HasCallStack => Connection -> IO ()
t3 connection = do
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
  print "Multithreading: Ok"

data ExampleData = MkExampleData
  { a1 :: Int64
  }
  deriving (Generic)
  deriving anyclass (ClickHaskell ExampleColumns)


type ExampleColumns =
 '[ Column "a1" Int64
  ]
