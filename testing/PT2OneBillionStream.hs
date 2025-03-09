{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

module PT2OneBillionStream (main) where

-- Internal
import ClickHaskell

-- GHC included
import Debug.Trace (traceMarkerIO)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)


main :: IO ()
main = do
  traceMarkerIO "Initialization" 
  let credentials = MkChCredential "default" "" "" "localhost" "9000"
  connection <- openNativeConnection credentials

  let totalRows = 100_000_000

  result <-
    sum <$>
      generateRandom
        @ExampleColumns
        @ExampleData
        connection
        1 10 2
        totalRows
        (pure . length)

  print $ "Processing done. " <> show result <> " rows was processed"


data ExampleData = MkExampleData
  { a1 :: Int64
  }
  deriving (Generic, Show, NFData)
  deriving anyclass (ReadableFrom (Columns ExampleColumns))


type ExampleColumns =
 '[ Column "a1" Int64
  ]
