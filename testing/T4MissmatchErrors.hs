{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , TypeApplications
  , ScopedTypeVariables
#-}

module T4MissmatchErrors where

-- Internal
import ClickHaskell

-- GHC included
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Exception (catch, throw, try)
import Control.Monad (void)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)


t4 :: HasCallStack => Connection -> IO ()
t4 connection = do
  res1 <-
    try (
      select
        @ExpectedColumns
        @ExpectedName
        connection
        (toChType "SELECT * FROM generateRandom('unexpectedName Int64', 1, 10, 2) LIMIT 1")
        pure
    )
  case res1 of
    Left (UserError (UnmatchedColumn _)) -> pure ()
    Right _ -> error "Expected an error, but got success"
    Left  e -> error ("MissmatchErrors: " <> show e)

  res2 <-
    try (
      select
        @ExpectedColumns
        @ExpectedName
        connection
        (toChType "SELECT * FROM generateRandom('expectedName UInt64', 1, 10, 2) LIMIT 1")
        pure
    )
  case res2 of
    Left (UserError (UnmatchedType _)) -> pure ()
    Right _ -> error "Expected an error, but got success"
    Left  e -> error ("MissmatchErrors: " <> show e)

  print "MissmatchErrors: Ok"


data ExpectedName = MkExpectedName
  { expectedName :: Int64
  }
  deriving (Generic)
  deriving anyclass (ReadableFrom (Columns ExpectedColumns))


type ExpectedColumns =
 '[ Column "expectedName" Int64
  ]
