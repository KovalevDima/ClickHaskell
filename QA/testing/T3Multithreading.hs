{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module T3Multithreading where

import ClickHaskell

import Control.Concurrent.Async
import GHC.Generics (Generic)


t3 :: Connection -> IO ()
t3 connection = do
  replicateConcurrently_ 10000 (
    select
      @ExampleColumns
      @ExampleData
      connection
      (toChType "SELECT * FROM generateRandom('a1 Int64', 1, 10, 2) LIMIT 1")
    )
  print "Multithreading: Ok"

data ExampleData = MkExampleData
  { a1 :: ChInt64
  }
  deriving (Generic)
  deriving anyclass (ReadableFrom (Columns ExampleColumns))


type ExampleColumns =
 '[ Column "a1" ChInt64
  ]
