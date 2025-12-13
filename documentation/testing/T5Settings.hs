{-# LANGUAGE
  DataKinds,
  DeriveAnyClass,
  DeriveGeneric,
  DerivingStrategies,
  TypeApplications,
  OverloadedStrings
#-}

module T5Settings where

import ClickHaskell
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

t5 :: HasCallStack => Connection -> IO ()
t5 connection = do
  _ <- select query connection pure
  putStrLn "t005: Ok"

query :: Select TestColumns TestData
query =
  passSettings
    ( id
      . addSetting @"max_threads_for_indexes" 8
      . addSetting @"max_local_write_bandwidth" 4096
      . addSetting @"default_view_definer" "default"
      . addSetting @"max_ast_depth" 1000
      . addSetting @"use_uncompressed_cache" True
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
