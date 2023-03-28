{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}
module Example.Select where

import ClickHaskell           (HttpChClient, initClient, ChCredential (..), httpStreamChSelect)
import ClickHaskell.TableDsl  (InDatabase)
import Example                (ExampleTable, ExampleData)

import Control.Concurrent     (threadDelay)


select :: IO ()
select = do

  -- 3. Init http client
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    Nothing

  dat <- httpStreamChSelect @ExampleData @(InDatabase "example" ExampleTable) client
  mapM_ print dat

  threadDelay 15_000_000
