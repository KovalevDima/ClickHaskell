{-# LANGUAGE
    BangPatterns
  , DataKinds
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , MultiParamTypeClasses
  , FlexibleInstances
#-}

module Main
  ( main
  ) where


-- Internal
import ClickHaskell.Buffering (forkBufferFlusher, DefaultBuffer, IsBuffer(..))
import ClickHaskell.Client    (interpretClient, IsChClient(..), HttpChClient, Writing, ChCredential(..))
import ClickHaskell.Generics  (WritableInto, ReadableFrom)
import ClickHaskell.Tables    (interpretTable, Table, Column)
import ClickHouse.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , LowCardinality, Nullable
  )


-- GHC included
import Data.ByteString (StrictByteString)
import Data.Int        (Int32, Int64)
import Data.Word       (Word32, Word64)
import GHC.Generics    (Generic)

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

exampleCredentials :: ChCredential
exampleCredentials = MkChCredential "default" "" "http://localhost:8123" "default"


type ExampleTable =
  Table
    "exampleWriteRead"
   '[ Column "a1" ChInt64
    , Column "a2" (LowCardinality ChString)
    , Column "a3" ChDateTime
    , Column "a4" ChUUID
    , Column "a5" ChInt32
    , Column "a6" (LowCardinality (Nullable ChString))
    , Column "a7" (LowCardinality ChString)
    ]

data ExampleData = MkExampleData
  { a1 :: ChInt64
  , a2 :: StrictByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: LowCardinality ChString
  }
  deriving (Generic, Show)

instance ReadableFrom ExampleTable ExampleData
instance WritableInto ExampleTable ExampleData

exampleDataSample :: ExampleData
exampleDataSample = MkExampleData
  { a1 = toChType (42 :: Int64)
  , a2 = "text"
  , a4 = toChType (0 :: Word64)
  , a3 = 42
  , a5 = 42
  , a6 = Just "500"
  , a7 = ""
  }
