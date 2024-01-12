{-# LANGUAGE
    BangPatterns
  , DataKinds
  , GeneralizedNewtypeDeriving
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

module Main
  ( main
  ) where


-- Internal
import ClickHaskell.Buffering (forkBufferFlusher, DefaultBuffer, IsBuffer(..))
import ClickHaskell.Client    (interpretClient, IsChClient(..), HttpChClient, Writing)
import Examples               (ExampleTable, ExampleData, exampleDataSample, exampleCredentials)


-- GHC included
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad      (replicateM_)
import GHC.Natural        (Natural)


main :: IO ()
main = benchExecutable


bufferSize              = 5_000_000
rowsNumber              = 100_000
concurrentBufferWriters = 500
msBetweenBuffering      = 10
msPauseBetweenDbWrites  = 5_000_000


benchExecutable :: IO ()
benchExecutable  = do

  print "1. Initializing client"
  client <- initClient @HttpChClient
    exampleCredentials
    Nothing

  print "2. Creating buffer"
  buffer <- createSizedBuffer @DefaultBuffer bufferSize

  print "3. Starting buffer flusher"
  !_ <- forkBufferFlusher
    msPauseBetweenDbWrites
    buffer
    print
    ( \dataList
      -> do
      print "Starting writing to database"
      _ <- interpretClient
        @(Writing ExampleData -> ExampleTable)
        client
        dataList
      print "Writing completed"
    )

  -- Construct or get some data
  let exampleData = exampleDataSample

  print "4. Writing to buffer"
  replicateM_ concurrentBufferWriters
    . (forkIO . replicateM_ rowsNumber)
    . (\someData -> do
        writeToSizedBuffer buffer someData
        threadDelay msBetweenBuffering
      )
    $ exampleData

  threadDelay 60_000_000
