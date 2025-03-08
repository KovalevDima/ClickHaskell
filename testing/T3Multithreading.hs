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
    generateRandom
      @ExampleColumns
      @ExampleData
      connection
      5
      pure
    )
  print "Multithreading: Ok"
  -- (toChType "SELECT * FROM generateRandom('a1 Int64', 1, 10, 2) LIMIT 1")

data ExampleData = MkExampleData
  { a1 :: Int64
  }
  deriving (Generic)
  deriving anyclass (ReadableFrom (Columns ExampleColumns))


type ExampleColumns =
 '[ Column "a1" Int64
  ]
