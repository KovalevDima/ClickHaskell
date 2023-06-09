# ClickHaskell
### Haskell interface for integration with ClickHouse

Package still under developing and doesn't have stable interface 


### Example of application that uses library functionality for development and profiling

```haskell
{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}

module Main
  ( main
  ) where

import ClickHaskell

-- GHC included libraries imports
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad      (replicateM_, void)
import Data.Text          (Text)

settings :: BenchSettings
settings = BenchSettings
  { sBufferSize        = 5_000_000
  , sConcurentWriters  = 500
  , sRowsPerWriter     = 100_000
  , sMsBetweenWrites   = 10
  , sMsBetweenChWrites = 1_000_000
  }

main :: IO ()
main = benchExecutable settings


-- 0. Settings for test

newtype ConcurrentBufferWriters   = ConcurrentBufferWriters   Int deriving newtype (Num)
newtype RowsPerBufferWriter       = RowsPerBufferWriter       Int deriving newtype (Num)
newtype MsBetweenBufferWrites     = MsBetweenBufferWrites     Int deriving newtype (Num)
newtype MsBetweenClickHouseWrites = MsBetweenClickHouseWrites Int deriving newtype (Num)


data BenchSettings = BenchSettings
  { sBufferSize        :: BufferSize
  , sConcurentWriters  :: ConcurrentBufferWriters
  , sRowsPerWriter     :: RowsPerBufferWriter
  , sMsBetweenWrites   :: MsBetweenBufferWrites
  , sMsBetweenChWrites :: MsBetweenClickHouseWrites
  }


-- 1. Describe table
type ExampleTable =
  Table
    "example"
    '[ DefaultColumn "string"   (LowCardinality ChString)
     , DefaultColumn "int64"    ChInt64
     , DefaultColumn "dateTime" ChDateTime
     , DefaultColumn "uuid"     ChUUID
     ]
    MergeTree
    '["string", "int64"]
    '["string"]

-- 2. Separate data you will work with
data ExampleData = ExampleData
  { string   :: ChString
  , int64    :: ChInt64
  , dateTime :: ChDateTime
  , uuid     :: ChUUID
  }
  deriving (Generic, HasChSchema, Show)

benchExecutable :: BenchSettings -> IO ()
benchExecutable (
  BenchSettings
     bufferSize
     (ConcurrentBufferWriters   concurrentBufferWriters)
     (RowsPerBufferWriter       rowsNumber             )
     (MsBetweenBufferWrites     msBetweenBufferWrites  )
     (MsBetweenClickHouseWrites msBetweenChWrites      )
  ) = do

  -- 3. Init clienthttpStreamChInsert client bufferData
  client <- initClient @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    (Just defaultHttpClientSettings)

  -- 4. Create database and table
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  -- 5. Create buffer 
  (buffer :: DefaultBuffer ExampleData) <- createSizedBuffer bufferSize

  -- 6. Start buffer flusher
  _ <- forkBufferFlusher
    (fromIntegral msBetweenChWrites)
    buffer
    print
    (void . httpStreamChInsert @(InDatabase "example" ExampleTable) client)

  -- 7. Get some data
  let _dataExample = ExampleData
        { string   = toChType @ChString   ("text"   :: Text)
        , int64    = toChType @ChInt64    42
        , dateTime = toChType @ChDateTime (500 :: Word32)
        , uuid     = toChType @ChUUID     nilChUUID
        }

  -- 8. Write something to buffer
  _threadId <-
    replicateM_ concurrentBufferWriters . forkIO
      . replicateM_ rowsNumber
      $ (\someData -> writeToSizedBuffer buffer someData >> threadDelay msBetweenBufferWrites) _dataExample

  threadDelay 60_000_000

```
