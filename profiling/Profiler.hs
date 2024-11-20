{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

module Profiler (main) where

import ClickHaskell
-- Internal
import ClickHaskell.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , LowCardinality, Nullable
  , Column(..), Columns
  )

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
  let credentials = MkChCredential "default" "" "" "localhost" "9000"
  connection <- openNativeConnection credentials

  let totalRows = 1_000_000

  threadDelay 250_000
  traceMarkerIO "Push data"

  traceMarkerIO "Starting reading"
  selectedData <-
    select
      @ExampleColumns
      @ExampleData
      connection
      {- FIXME:
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
  print $ "Writing done. " <> show totalRows <> " rows was written"
  threadDelay 1_000_000


data ExampleData = MkExampleData
  { a1 :: ChInt64
  {- FIXME:
  , a2 :: StrictByteString
  , a6 :: Nullable ChString
  , a7 :: ChString
  -}
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  }
  deriving (Generic, Show)
  deriving anyclass (ReadableFrom (Columns ExampleColumns), WritableInto (Table "exampleWriteRead" ExampleColumns))


type ExampleColumns =
 '[ Column "a1" ChInt64
  {- FIXME:
  , Column "a2" (LowCardinality ChString)
  , Column "a6" (LowCardinality (Nullable ChString))
  , Column "a7" (LowCardinality ChString)
  -}
  , Column "a3" ChDateTime
  , Column "a4" ChUUID
  , Column "a5" ChInt32
  ]
