{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

module OneBillionStream (main) where

import ClickHaskell
-- Internal
import ClickHaskell.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , LowCardinality, Nullable
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
import Control.DeepSeq (($!!), NFData)


main :: IO ()
main = do
  traceMarkerIO "Initialization"  
  let credentials = MkChCredential "default" "" "" "localhost" "9000"
  connection <- openNativeConnection credentials

  let totalRows = 1_000_000_000

  threadDelay 250_000
  traceMarkerIO "Push data"

  traceMarkerIO "Starting reading"
  result <-
    streamSelect
      @ExampleColumns
      @ExampleData
      connection
      (toChType $
      "SELECT * FROM generateRandom('\
      \a1 Int64, \
      \a2 String, \
      \a3 DateTime, \
      \a4 UUID, \
      \a5 Int32, \
      \a6 Nullable(String), \
      \a7 String \
      \', 1, 10, 2) LIMIT " <> (string8 . show) totalRows)
      (\records -> pure $!! [length records])
  print result

  traceMarkerIO "Completion"
  print $ "Processing done. " <> show totalRows <> " rows was processed"
  threadDelay 1_000_000


data ExampleData = MkExampleData
  { a1 :: ChInt64
  , a2 :: StrictByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: ChString
  }
  deriving (Generic, Show, NFData)
  deriving anyclass (ReadableFrom (Columns ExampleColumns), WritableInto (Table "oneBillionStream" ExampleColumns))


type ExampleColumns =
 '[ Column "a1" ChInt64
  , Column "a2" ChString
  , Column "a3" ChDateTime
  , Column "a4" ChUUID
  , Column "a5" ChInt32
  , Column "a6" (Nullable ChString)
  , Column "a7" ChString
  ]
