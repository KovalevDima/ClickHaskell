{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , NumericUnderscores
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , ScopedTypeVariables
#-}
module Example.Select where

import ClickHaskell              (HttpChClient, initClient, ChCredential (..), httpStreamChSelect)
import ClickHaskell.TableDsl     (InDatabase, Sampled)

-- 1. Describe table and queryable data
import Example (ExampleTable, ExampleData)





select :: IO ()
select = do

  -- 2. Init http client
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    Nothing

  -- 3. Perform select
  dat <- httpStreamChSelect @(Sampled "fieldName" "" ExampleData) @(InDatabase "example" ExampleTable) client

  -- 4. Handle data
  mapM_ print dat
