{-# LANGUAGE
  OverloadedStrings 
#-}
module Test.Headers where

import Data.Bool (bool)
import Data.ByteString (StrictByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import ClickHaskell.Client (ProfileData, parseSummary, ProfileData(..))

headersPartingTest :: TestTree
headersPartingTest = testGroup "Headers" summaryHeaderTest


summaryHeaderTest :: [TestTree]
summaryHeaderTest =
  [ testCase "Summary" $
      assertBool
        "Parsing result doesn't match excepted parsing result"
        (expectedParsingResult == parsedSummary)
  ]


parsedSummary :: ProfileData
parsedSummary = parseSummary summaryExample

expectedParsingResult :: ProfileData
expectedParsingResult = MkProfileData
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
