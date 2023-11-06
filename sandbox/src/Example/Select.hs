{-# LANGUAGE
    ExplicitNamespaces
  , DataKinds
  , OverloadedStrings
#-}

module Example.Select where

import ClickHaskell.Client   (defaultHttpClientSettings, httpStreamChSelect, setHttpClientTimeout, ChClient(..), ChCredential(..), HttpChClient)
import ClickHaskell.DataDsl  (EqualTo, Variable, Result, type(%%), constructSelection)
import ClickHaskell.DbTypes  (toCh, Text)
import Example               (ExampleTable, ExampleData)


select :: IO ()
select = do

  print "1. Initializing http client"
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123" "exampleDb")
    (Just $ setHttpClientTimeout 6_000_000 defaultHttpClientSettings)

  print "2. Performing select"
  dat <-
    httpStreamChSelect
      client
      $ constructSelection
        @ExampleTable
        @(Result ExampleData
          %% EqualTo "a2" Variable
          %% EqualTo "a3" "value2"
        )
        (toCh @Text "value1")

  print "3. Handling data"
  print $ length dat
