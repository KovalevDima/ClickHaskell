{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

module ProfilerV2 (main) where

-- External
import Network.HTTP.Client (defaultManagerSettings, newManager)

import ClickHaskell
-- Internal
import ClickHaskell.Tables (Table, Columns(..), appendColumn)
import ClickHaskell.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , LowCardinality, Nullable
  )
import ClickHaskell.NativeProtocol.Serialization (latestSupportedRevision)
import ClickHaskell.NativeProtocol.Columns (Column(..))

-- GHC included
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever, replicateM_)
import Data.ByteString (StrictByteString)
import Data.ByteString.Builder (string8)
import Data.IORef (atomicModifyIORef, newIORef)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Debug.Trace (traceMarkerIO)
import GHC.Conc (atomically, newTVarIO, readTVarIO)
import GHC.Generics (Generic)
import GHC.Natural (Natural)


main :: IO ()
main = do
  traceMarkerIO "Initialization"
  connection <- openNativeConnection devCredential
  let totalRows = 1_000_000

  threadDelay 250_000
  traceMarkerIO "Push data"

  traceMarkerIO "Starting reading"
  selectedData <-
    select
      @ExampleColumns
      @ExampleData
      connection
      {-
      \a2 LowCardinality(String), \
      \a6 LowCardinality(Nullable(String)), \
      \a7 LowCardinality(String)\
      -}
      (toChType $
      "SELECT * FROM generateRandom('\
      \a1 Int64, \
      \a3 DateTime, \
      \a4 UUID, \
      \a5 Int32 \
      \', 1, 10, 2) LIMIT " <> (string8 . show) totalRows)

  threadDelay 1_000_000
  traceMarkerIO "Starting writing"
  insertInto
    @(Table "exampleWriteRead" ExampleColumns)
    connection
    selectedData

  traceMarkerIO "Completion"
  print $ "Writing done. " <> show (length selectedData) <> " rows was written"
  threadDelay 1_000_000


data ExampleData = MkExampleData
  { a1 :: ChInt64
  -- , a2 :: StrictByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  -- , a6 :: Nullable ChString
  -- , a7 :: ChString
  }
  deriving (Generic, Show)
  deriving anyclass (ReadableFrom (Columns ExampleColumns), WritableInto (Table "exampleWriteRead" ExampleColumns))


type ExampleColumns =
 '[ Column "a1" ChInt64
  -- , Column "a2" (LowCardinality ChString)
  , Column "a3" ChDateTime
  , Column "a4" ChUUID
  , Column "a5" ChInt32
  -- , Column "a6" (LowCardinality (Nullable ChString))
  -- , Column "a7" (LowCardinality ChString)
  ]

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
