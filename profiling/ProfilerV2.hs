{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  print "Connected"
  ping connection
  print "Pinged"
  _a <- selectFrom @ExampleTable @ExampleData connection
  print "Dummy queries done"
  insertInto @ExampleTable connection devColumns

  let totalRows = 1_000_000

  threadDelay 1_000_000
  traceMarkerIO "Push data"

  traceMarkerIO "Starting reading"
  -- selectedData <-
  --   select
  --     @ExampleColumns
  --     @ExampleData
  --     manager
  --     credentials
  --     ("SELECT * FROM generateRandom('\
  --     \a1 Int64, \
  --     \a2 LowCardinality(String), \
  --     \a3 DateTime, \
  --     \a4 UUID, \
  --     \a5 Int32, \
  --     \a6 LowCardinality(Nullable(String)), \
  --     \a7 LowCardinality(String)\
  --     \', 1, 10, 2) LIMIT " <> (string8 . show) totalRows)

  threadDelay 1_000_000
  traceMarkerIO "Starting writing"

  traceMarkerIO "Completion"
  print $ "5. Writing done. " <> show totalRows <> " rows was written"
  threadDelay 1_000_000

-- devColumns ::  Columns '[Column "var2" ChUInt32, Column "var" ChUInt32]
devColumns ::  [ExampleData]
devColumns = MkExample <$> replicate (1*1000*1000) 127

type ExampleTable = Table "example" '[Column "val" ChInt64]

data ExampleData = MkExample
  { val :: Int64
  } deriving (Generic)

instance ReadableFrom ExampleTable ExampleData
instance WritableInto ExampleTable ExampleData


devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
