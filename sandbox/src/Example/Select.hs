{-# LANGUAGE
    ExplicitNamespaces
  , DataKinds
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Example.Select where

import ClickHaskell.Client  (defaultHttpClientSettings, httpStreamChSelect, setHttpClientTimeout, ChClient(..), ChCredential(..), HttpChClient, ClientResponse)
import ClickHaskell.DataDsl (EqualTo, Variable, Result, type(%%), constructSelection)
import ClickHaskell.DbTypes (toCh, Text)
import Example              (ExampleTable, ExampleData)


select :: IO (ClientResponse [ExampleData])
select = do

  -- 1. Initializing http client
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123" "exampleDb")
    (Just $ setHttpClientTimeout 6_000_000 defaultHttpClientSettings)

  -- 2. Performing select
  httpStreamChSelect
    client
    $ constructSelection
      @ExampleTable
      @(Result ExampleData
        %% EqualTo "a2" Variable
        %% EqualTo "a1" "42"
      )
      (toCh @Text "text")
