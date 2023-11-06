{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

module Example.Insert where

import Control.Monad (void)

import ClickHaskell.Client (httpStreamChInsert, ChClient(..), ChCredential(..), HttpChClient)
import Example             (ExampleTable, dataExample)


insert :: IO ()
insert = do

  print "1. Init client"
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123" "exampleDb")
    Nothing

  void $ httpStreamChInsert @ExampleTable client [dataExample]
