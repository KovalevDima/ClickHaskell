{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

module Bench
  ( benchExecutable
  , BenchSettings(..)
  ) where

-- Internal dependencies
import ClickHaskell
import Example      (ExampleTable, ExampleData(..), dataExample)

-- GHC included libraries imports
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad      (replicateM_, void)


-- 0. Settings for test

newtype ConcurrentBufferWriters   = ConcurrentBufferWriters   Int deriving Num
newtype RowsPerBufferWriter       = RowsPerBufferWriter       Int deriving Num
newtype MsBetweenBufferWrites     = MsBetweenBufferWrites     Int deriving Num
newtype MsBetweenClickHouseWrites = MsBetweenClickHouseWrites Int deriving Num


data BenchSettings = BenchSettings
  { sBufferSize        :: BufferSize
  , sConcurentWriters  :: ConcurrentBufferWriters
  , sRowsPerWriter     :: RowsPerBufferWriter
  , sMsBetweenWrites   :: MsBetweenBufferWrites
  , sMsBetweenChWrites :: MsBetweenClickHouseWrites
  }

-- 1. Create our schema haskell representation

benchExecutable :: BenchSettings -> IO ()
benchExecutable (
  BenchSettings
     bufferSize
     (ConcurrentBufferWriters   concurrentBufferWriters)
     (RowsPerBufferWriter       rowsNumber             )
     (MsBetweenBufferWrites     msBetweenBufferWrites  )
     (MsBetweenClickHouseWrites msBetweenChWrites      )
  ) = do

  -- 2. Init clienthttpStreamChInsert client bufferData
  client <- initClient @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    (Just defaultHttpClientSettings)

  -- 3. Create database and table
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  -- 4. Create buffer 
  (buffer :: DefaultBuffer ExampleData) <- createSizedBuffer bufferSize

  -- 5. Start buffer flusher
  print "Writing data"
  _ <- forkBufferFlusher @(InDatabase "example" ExampleTable)
    (fromIntegral msBetweenChWrites)
    buffer
    print
    (void . httpStreamChInsert @(InDatabase "example" ExampleTable) client)

  -- 6. Get some data
  let dataExample' = dataExample

  -- 7. Write something to buffer
  _threadId <-
    replicateM_ concurrentBufferWriters . forkIO
      . replicateM_ rowsNumber
      $ (\someData -> writeToSizedBuffer buffer someData >> threadDelay msBetweenBufferWrites) dataExample'

  threadDelay 60_000_000
