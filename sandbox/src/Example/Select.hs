{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

{-# OPTIONS_GHC -fprint-potential-instances #-}

module Example.Select where

-- Internal dependencies
import ClickHaskell
import Example      (ExampleTable, ExampleData)


select :: IO ()
select = do

  -- 1. Init http client
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    (Just $ setHttpClientTimeout 6000000 defaultHttpClientSettings)

  -- 2. Create database and table
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  -- 3. Perform select
  print "Reading data"
  dat <-
      httpStreamChSelect @(("a2" `SuchThat` HasInfix "") ExampleData) @(InDatabase "example" ExampleTable) client

  -- 4. Handle data
  print $ length dat
