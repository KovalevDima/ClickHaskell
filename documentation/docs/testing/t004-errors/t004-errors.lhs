<div>
  <p>
    1. Runs queries with types and names missmatch and handles error
  </p>
  <p>
    You can manually run database and tests:
  </p>
</div>

```haskell
{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , TypeApplications
  , ScopedTypeVariables
  , OverloadedStrings
#-}

module Main (main) where

-- Internal
import ClickHaskell

-- GHC included
import Control.Exception (try)
import GHC.Generics (Generic)


main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  connOld <- openConnection (overrideMaxRevision 1 defaultConnectionArgs)

  validateUnmatchedColumn connection
  validateUnmatchedColumn connOld

  validateUnmatchedType connection
  validateUnmatchedType connOld

  validateUnmatchedColumnsCount connection
  validateUnmatchedColumnsCount connOld

  putStrLn "MissmatchErrors: Ok"


validateUnmatchedColumn :: Connection -> IO ()
validateUnmatchedColumn connection = do
  res1 <-
    try (
      select
        (unsafeMkSelect
          @TestExpectedColumns
          @ExpectedName
          (\_cols -> "SELECT * FROM generateRandom('unexpectedName Int64', 1, 10, 2) LIMIT 1")
        )
        connection
        pure
    )
  case res1 of
    Left (UnmatchedResult (UnmatchedColumn _)) -> pure ()
    Right _ -> error "Expected an error, but got success"
    Left  e -> error ("MissmatchErrors: " <> show e)


validateUnmatchedType :: Connection -> IO ()
validateUnmatchedType connection = do
  res2 <-
    try (
      select
        (unsafeMkSelect
          @TestExpectedColumns
          @ExpectedName
          (\_cols -> "SELECT * FROM generateRandom('expectedName UInt64', 1, 10, 2) LIMIT 1")
        )
        connection
        pure
    )
  case res2 of
    Left (UnmatchedResult (UnmatchedType _)) -> pure ()
    Right _ -> error "Expected an error, but got success"
    Left  e -> error ("MissmatchErrors: " <> show e)


validateUnmatchedColumnsCount :: Connection -> IO ()
validateUnmatchedColumnsCount connection = do
  res3 <-
    try (
      select
        (unsafeMkSelect
          @TestExpectedColumns
          @ExpectedName
          (\_cols -> "SELECT * FROM generateRandom('expectedName Int64, unexpectedColumn Int64', 1, 10, 2) LIMIT 1")
        )
        connection
        pure
    )
  case res3 of
    Left (UnmatchedResult (UnmatchedColumnsCount _)) -> pure ()
    Right _ -> error "Expected an error, but got success"
    Left  e -> error ("MissmatchErrors: " <> show e)


data ExpectedName = MkExpectedName
  { expectedName :: Int64
  }
  deriving (Generic)
  deriving anyclass (ClickHaskell TestExpectedColumns)


type TestExpectedColumns =
 '[ Column "expectedName" Int64
  ]
```
