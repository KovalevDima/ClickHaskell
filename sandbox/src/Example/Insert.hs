{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TypeApplications
#-}

module Example.Insert where

import ClickHaskell.Client (httpStreamChInsert, ChClient(..), ChCredential(..), HttpChClient, ClientResponse)
import Example             (ExampleTable, dataExample)


insert :: IO (ClientResponse ())
insert = do

  -- 1. Init client
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123" "exampleDb")
    Nothing

  httpStreamChInsert @ExampleTable client [dataExample]
