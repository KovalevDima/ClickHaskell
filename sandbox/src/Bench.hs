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


-- Settings for test
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

benchExecutable :: BenchSettings -> IO ()
benchExecutable (
  BenchSettings
     bufferSize
     (ConcurrentBufferWriters   concurrentBufferWriters)
     (RowsPerBufferWriter       rowsNumber             )
     (MsBetweenBufferWrites     msBetweenBufferWrites  )
     (MsBetweenClickHouseWrites msBetweenChWrites      )
  ) = do

  print "1. Initializing client"
  client <- initClient @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    (Just defaultHttpClientSettings)

  print "2. Bootstrapping DB"
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  print "3. Creating buffer"
  (buffer :: DefaultBuffer ExampleData) <- createSizedBuffer bufferSize

  print "4. Starting buffer flusher"
  _ <- forkBufferFlusher @(InDatabase "example" ExampleTable)
    (fromIntegral msBetweenChWrites)
    buffer
    print
    ( \dataList
      -> print "Starting writing to database"
      >> void (httpStreamChInsert @(InDatabase "example" ExampleTable) client dataList)
      >> print "Writing completed"
    )

  -- Construct or get some data
  let dataExample' = dataExample

  print "5. Writing to buffer"
  _threadId <-
    replicateM_ concurrentBufferWriters . forkIO
      . replicateM_ rowsNumber
      $ (\someData -> writeToSizedBuffer buffer someData >> threadDelay msBetweenBufferWrites) dataExample'

  threadDelay 60_000_000
