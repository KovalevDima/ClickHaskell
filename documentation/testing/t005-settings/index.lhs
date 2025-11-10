```haskell
{-# OPTIONS_GHC -freduction-depth=1200 #-}
module Main (main) where

import ClickHaskell
import GHC.Generics


main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs

  let
    sampleQuery =
      fromGenerateRandom
        @TestColumns
        @TestData
        (1, 10, 2)
        1
    addSampleSettings =
      passSettings (
        addSetting @"max_threads_for_indexes" 8 .
        addSetting @"max_local_write_bandwidth" 4096 .
        addSetting @"default_view_definer" "default" .
        addSetting @"max_ast_depth" 1000 .
        id
      )

  _res <-
    select
      (addSampleSettings sampleQuery)
      connection
      pure

  putStrLn "t005: Ok"

data TestData = MkTestData
  { testCol :: Int32
  }
  deriving (Generic, Show)
  deriving anyclass (ClickHaskell TestColumns)

type TestColumns =
 '[ Column "testCol" Int32
  ]
```
