{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

module Example.Insert where

-- Internal dependencies
import ClickHaskell
import Example      (ExampleTable, dataExample)

-- GHC included libraries imports
import Control.Concurrent (threadDelay)
import Control.Monad      (void)


insert :: IO ()
insert = do

  print "1. Init client"
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    Nothing

  -- 2. Create db and table
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  print "3. Create buffer" 
  buffer <- createSizedBuffer @DefaultBuffer 500_000

  print "4. Start buffer flusher"
  _ <- forkBufferFlusher @(InDatabase "example" ExampleTable)
    5_000_000
    buffer
    print
    (void . httpStreamChInsert @(InDatabase "example" ExampleTable) client)

  -- Construct or get some data
  let dataExample' = dataExample

  print "5. Writing data to buffer"
  writeToSizedBuffer buffer dataExample'

  threadDelay 15_000_000
