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

module ProfilerV3
  ( main
  ) where

-- External
import Network.HTTP.Client (defaultManagerSettings, newManager)

-- Internal
import ClickHaskell.ClientV3  (runWriteTable, ChCredential(..), initClient)
import ClickHaskell.Generics  (WritableInto, ReadableFrom)
import ClickHaskell.Tables    (interpretTable, Table, Column)
import ClickHouse.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , LowCardinality, Nullable
  )


-- GHC included
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (modifyTVar, newTQueueIO, writeTQueue, flushTQueue)
import Control.Monad (forever, replicateM_)
import Data.ByteString (StrictByteString)
import Data.IORef (atomicModifyIORef, newIORef)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Debug.Trace (traceMarkerIO)
import GHC.Conc (atomically, newTVarIO, readTVarIO)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Data.Typeable (Proxy(..))


main :: IO ()
main = do
  traceMarkerIO "Initialization"
  client <- initClient exampleCredentials defaultManagerSettings

  let totalRows = 1_000_000
  
  threadDelay 1_000_000
  traceMarkerIO "Push data"
  writingDataQueue <- newTQueueIO
  (replicateM_ totalRows . atomically . writeTQueue writingDataQueue)
    exampleDataSample
  
  threadDelay 1_000_000
  traceMarkerIO "Starting writing"
  runWriteTable
    client
    (Proxy @ExampleTable)
    =<< atomically (flushTQueue writingDataQueue)

  traceMarkerIO "Completion"
  print $ "5. Writing done. " <> show totalRows <> " rows was written"
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
