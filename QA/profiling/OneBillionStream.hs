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

-- Internal
import ClickHaskell

-- GHC included
import Control.Concurrent (threadDelay)
import Data.ByteString.Builder (string8)
import Debug.Trace (traceMarkerIO)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)


main :: IO ()
main = do
  traceMarkerIO "Initialization" 
  let credentials = MkChCredential "default" "" "" "localhost" "9000"
  connection <- openNativeConnection credentials

  let totalRows = 1_000_000_000 :: Integer

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
        \a1 Int64 \
        \', 1, 10, 2) LIMIT " <> (string8 . show) totalRows
      )
      (\records -> pure [length records])
  print $ sum result

  traceMarkerIO "Completion"
  print $ "Processing done. " <> show totalRows <> " rows was processed"
  threadDelay 1_000_000


data ExampleData = MkExampleData
  { a1 :: ChInt64
  }
  deriving (Generic, Show, NFData)
  deriving anyclass (ReadableFrom (Columns ExampleColumns))


type ExampleColumns =
 '[ Column "a1" ChInt64
  ]
