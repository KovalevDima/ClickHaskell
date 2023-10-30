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

  print "2. Performing select"
  dat <-
    httpStreamChSelect
      client
      $ constructSelection
        @(InDatabase "example" ExampleTable)
        @(Result ExampleData
          %% EqualTo "a2" "Variable"
          %% EqualTo "a3" "Variable"
        )

  print "3. Handling data"
  print $ length dat
