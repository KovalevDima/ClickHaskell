{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

module Bench
  ( benchExecutable
  , BenchSettings(..)
  ) where

import ClickHaskell.Buffering (forkBufferFlusher, BufferSize, DefaultBuffer, IsBuffer(..))
import ClickHaskell.Client    (defaultHttpClientSettings, httpStreamChInsert, ChClient(..), ChCredential(..))
import Example                (ExampleTable, dataExample)

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
  client <- initClient
    (ChCredential "default" "" "http://localhost:8123" "example")
    (Just defaultHttpClientSettings)

  print "2. Creating buffer"
  buffer <- createSizedBuffer @DefaultBuffer bufferSize

  print "3. Starting buffer flusher"
  !_ <- forkBufferFlusher
    (fromIntegral msBetweenChWrites)
    buffer
    print
    ( \dataList
      -> print "Starting writing to database"
      >> void (httpStreamChInsert @ExampleTable client dataList)
      >> print "Writing completed"
    )

  -- Construct or get some data
  let dataExample' = dataExample

  print "4. Writing to buffer"
  !_threadId <-
    replicateM_ concurrentBufferWriters . forkIO
      . replicateM_ rowsNumber
      . (\someData
        -> writeToSizedBuffer buffer someData
        >> threadDelay msBetweenBufferWrites
        )
      $ dataExample'

  threadDelay 60_000_000
