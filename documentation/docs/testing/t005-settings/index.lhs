```haskell
{-# LANGUAGE
  DataKinds,
  DeriveAnyClass,
  DeriveGeneric,
  DerivingStrategies,
  TypeApplications,
  OverloadedStrings
#-}

module Main (main) where

import ClickHaskell
import GHC.Generics (Generic)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  connOld <- openConnection (overrideMaxRevision 1 defaultConnectionArgs)

  runSettings connection
  runSettings connOld
  putStrLn "Ok"


runSettings :: Connection -> IO ()
runSettings conn = do
  const () <$> select query conn pure


query :: Select TestColumns TestData
query =
  passSettings
    ( id
      . addSetting @"max_threads_for_indexes" 8
      . addSetting @"max_local_write_bandwidth" 4096
      . addSetting @"default_view_definer" "default"
      . addSetting @"max_ast_depth" 1000
      . addSetting @"use_uncompressed_cache" True
      . addSetting @"delta_lake_snapshot_version" (-5)
    )
    (fromGenerateRandom @TestColumns @TestData (1, 10, 2) 1)

data TestData = MkTestData
  { testCol :: Int32
  }
  deriving (Generic, Show)
  deriving anyclass (ClickHaskell TestColumns)

type TestColumns =
 '[ Column "testCol" Int32
  ]
```
