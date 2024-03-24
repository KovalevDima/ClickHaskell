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
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.STM (modifyTVar)
import Control.Monad (replicateM_, forever)
import Data.IORef (newIORef, atomicModifyIORef)
import GHC.Conc (newTVarIO, atomically, readTVarIO)
import GHC.Natural (Natural)
import Debug.Trace (traceMarkerIO)


main :: IO ()
main = do
  insertedCounter <- newTVarIO 0

  traceMarkerIO "Initialization"
  print "1. Initializing client"
  client <-
    initClient
      @HttpChClient
      exampleCredentials
      Nothing

  print "2. Creating buffer"
  buffer <-
    createSizedBuffer
      @DefaultBuffer
      5_000_000

  print "3. Starting buffer flusher"
  writerThread <-
    forkBufferFlusher
      5_000_000
      buffer
      print
      (\dataList -> do
        traceMarkerIO "Writing"
        interpretClient
          @(Writing ExampleData -> ExampleTable)
          client
          dataList
        (atomically . modifyTVar insertedCounter) (+ length dataList) 
      )

  let writingThreads = 2
  print ("4. Writing to buffer in " <> show writingThreads <> " threads")
  replicateM_
    writingThreads
    (forkIO . forever $ do
      replicateM_ 1_000 (writeToSizedBuffer buffer exampleDataSample)
      threadDelay 500_000
    )

  threadDelay 60_000_000
  traceMarkerIO "Completion"
  killThread writerThread

  totalInserted <- readTVarIO insertedCounter
  print $ "5. Writing done. " <> show totalInserted <> " rows was written"
  threadDelay 1_000_000
