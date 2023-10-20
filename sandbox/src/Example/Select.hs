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

  print "1. Initializing http client"
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    (Just $ setHttpClientTimeout 6_000_000 defaultHttpClientSettings)

  print "2. Creating database and table"
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  print "3. Performing select"
  dat <-
    httpStreamChSelect
      @(("a2" `SuchThat` HasInfix "") ExampleData)
      @(InDatabase "example" ExampleTable)
      client

  print "4. Handling data"
  print $ length dat
