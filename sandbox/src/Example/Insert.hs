{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}

module Example.Insert where

-- Internal dependencies
import ClickHaskell
import Example      (ExampleTable, ExampleData(..))

-- GHC included libraries imports
import Data.Text              (Text)
import Control.Concurrent     (threadDelay)
import Control.Monad          (void)


insert :: IO ()
insert = do

  -- 1. Init clienthttpStreamChInsert client bufferData
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    Nothing

  -- 2. Create db and table
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  -- 3. Create buffer 
  (buffer :: DefaultBuffer ExampleData) <- createSizedBuffer 500_000

  print "Writing data"
  -- 4. Start buffer flusher
  _ <- forkBufferFlusher
    5_000_000
    buffer
    print
    (void . httpStreamChInsert @(InDatabase "example" ExampleTable) client)

  -- 5. Get some data
  let _dataExample = ExampleData
        { string   = toChType @(LowCardinality ChString)   $ ("text\t"   :: Text)
        , int64    = toChType @ChInt64                       42
        , dateTime = toChType @ChDateTime                  $ (500 :: Word32)
        , uuid     = toChType @ChUUID                        nilChUUID
        }

  -- 6. Write data to buffer
  writeToSizedBuffer buffer _dataExample

  threadDelay 15_000_000
