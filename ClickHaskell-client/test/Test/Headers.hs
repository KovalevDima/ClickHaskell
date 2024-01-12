{-# LANGUAGE
  OverloadedStrings 
#-}

{-# OPTIONS_GHC
  -Wno-deprecations
#-}

module Test.Headers
  ( headersPartingTest
  ) where


import Data.Bool (bool)
import Data.ByteString (StrictByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)


import ClickHaskell.Client (ClickHouseSummary(..), parseJsonSummary)


headersPartingTest :: TestTree
headersPartingTest = testGroup "Headers" summaryHeaderTest


summaryHeaderTest :: [TestTree]
summaryHeaderTest =
  [ testCase "Summary" $
      assertBool
        "Parsing result doesn't match excepted parsing result"
        (expectedParsingResult == parsedSummary)
  ]


parsedSummary :: ClickHouseSummary
parsedSummary = parseJsonSummary summaryExample

expectedParsingResult :: ClickHouseSummary
expectedParsingResult = MkClickHouseSummary
  { readRows = 1
  , readBytes = 78
  , writtenRows = 1
  , writtenBytes = 78
  , totalRowsToRead = 0
  , resultRows = 1
  , resultBytes = 78
  }

summaryExample :: StrictByteString
summaryExample =
  "{ \"read_rows\":\"1\"\
  \, \"read_bytes\":\"78\"\
  \, \"written_rows\":\"1\"\
  \, \"written_bytes\":\"78\"\
  \, \"total_rows_to_read\":\"0\"\
  \, \"result_rows\":\"1\"\
  \, \"result_bytes\":\"78\"\
  \}"
