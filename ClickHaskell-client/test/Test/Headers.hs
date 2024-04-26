{-# LANGUAGE
    DeriveFunctor
  , DeriveGeneric
  , OverloadedStrings
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC
  -Wno-deprecations
#-}

module Test.Headers
  ( headersPartingTest
  ) where

-- External
import Network.HTTP.Client (Response (..))
import Network.HTTP.Simple as H (getResponseHeaders)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)


-- GHC included
import Data.Bool (bool)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 as BS8 (notElem, readInteger, split)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

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




{- |
Wrapper with info from HTTP header @X-ClickHouse-Summary@
-}
data ChResponse result = MkChResponse
  { getResult :: result
  , profileData :: ClickHouseSummary
  } deriving (Generic, Functor)
deriving instance (Show result) => Show (ChResponse result)

{- | ToDocument
-}
data ClickHouseSummary = MkClickHouseSummary
  { readRows        :: Integer
  , readBytes       :: Integer
  , writtenRows     :: Integer
  , writtenBytes    :: Integer
  , totalRowsToRead :: Integer
  , resultRows      :: Integer
  , resultBytes     :: Integer
  } deriving (Generic, Show, Eq)




{- |
1. Takes an HTTP Response
2. Lookups @X-ClickHouse-Summary@ header
3. Parses it
-}
parseSummaryFromHeaders :: Response a -> ClickHouseSummary
parseSummaryFromHeaders
  = parseJsonSummary
  . fromMaybe (error "Can't find response header \"X-ClickHouse-Summary\". Please report an issue")
  . lookup "X-ClickHouse-Summary" . H.getResponseHeaders

{- |
ClickHouseSummary JSON parser. Takes raw byte sequence and parses it
-}
parseJsonSummary :: StrictByteString -> ClickHouseSummary
parseJsonSummary
  = reduce (MkClickHouseSummary (-1) (-1) (-1) (-1) (-1) (-1) (-1))
  . repack
  . filter
    (\bs
      -> BS8.notElem '}' bs
      && BS8.notElem '{' bs
      && BS8.notElem ';' bs
      && BS8.notElem ',' bs
      && BS8.notElem ':' bs
    )
  . BS8.split '"'
  where
  reduce :: ClickHouseSummary -> [(StrictByteString, Integer)] -> ClickHouseSummary
  reduce acc (("read_rows"         , integer):xs) = reduce acc{readRows       =integer} xs
  reduce acc (("read_bytes"        , integer):xs) = reduce acc{readBytes      =integer} xs
  reduce acc (("written_rows"      , integer):xs) = reduce acc{writtenRows    =integer} xs
  reduce acc (("written_bytes"     , integer):xs) = reduce acc{writtenBytes   =integer} xs
  reduce acc (("total_rows_to_read", integer):xs) = reduce acc{totalRowsToRead=integer} xs
  reduce acc (("result_rows"       , integer):xs) = reduce acc{resultRows     =integer} xs
  reduce acc (("result_bytes"      , integer):xs) = reduce acc{resultBytes    =integer} xs
  reduce acc [] = acc
  reduce acc ((_, _):xs) = reduce acc xs

  repack :: [StrictByteString] -> [(StrictByteString, Integer)]
  repack [] = []
  repack [_] = error "Can't parse json. Please report an issue"
  repack (t1:t2:xs) =
    ( t1
    , case BS8.readInteger t2 of
        Just (int, "") -> int
        Just (_, _) -> error "Can't parse int fully. Please report as issue"
        Nothing -> error "Can't parse int at all. Please report an issue"
    ) : repack xs
