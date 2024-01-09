{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DuplicateRecordFields
  , ExplicitNamespaces
  , FlexibleInstances
  , MultiParamTypeClasses
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

module Examples where

import ClickHaskell.Client     (performOperation, HttpChClient, ChCredential(..), ChClient(..), ChResponse, setHttpClientTimeout)
import ClickHaskell.Operations (ReadableFrom, WritableInto, Reading, ClickHouse, Writing, IsOperation(..), renderInsertHeader, renderSelectQuery)
import ClickHaskell.Tables     (Table, View, Column, Parameter)
import ClickHouse.DbTypes      (nilChUUID, ChDateTime, ChInt32, ChInt64, ChString, ChUUID, LowCardinality, Nullable, toChType)

import Data.ByteString (StrictByteString)
import Data.Int        (Int32, Int64)
import Data.Word       (Word32)
import GHC.Generics    (Generic)


write :: IO (ChResponse ())
write = do

  print "1. Initializing client"
  client <- initClient
    @HttpChClient
    exampleCredentials
    Nothing

  print "2. Performing client"
  performOperation
    @(Writing ExampleData -> ClickHouse ExampleTable)
    client
    [exampleDataSample]


read :: IO (ChResponse [ExampleData])
read = do

  print "1. Initializing client"
  client <- initClient
    @HttpChClient
    exampleCredentials
    (Just $ setHttpClientTimeout 500_000)

  print "2. Performing select"
  performOperation
    @(Reading ExampleData -> ClickHouse ExampleTable)
    client


exampleCredentials :: ChCredential
exampleCredentials = ChCredential "default" "" "http://localhost:8123" "exampleDb"


type ExampleTable =
  Table
    "example"
   '[ Column "a1" ChInt64
    , Column "a2" (LowCardinality ChString)
    , Column "a3" ChDateTime
    , Column "a4" ChUUID
    , Column "a5" ChInt32
    , Column "a6" (LowCardinality (Nullable ChString))
    , Column "a7" (LowCardinality ChString)
    ]
   '[]

type ExampleView =
  View
    "exampleView"
   '[ Column "a1" ChInt64
    , Column "a2" (LowCardinality ChString)
    , Column "a3" ChDateTime
    , Column "a4" ChUUID
    , Column "a5" ChInt32
    , Column "a6" (LowCardinality (Nullable ChString))
    , Column "a7" (LowCardinality ChString)
    ]
   '[ Parameter "param" ChString
    ]
   '[]

data ExampleData = MkExampleData
  { a1 :: ChInt64
  , a2 :: StrictByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: LowCardinality ChString
  } deriving (Generic, Show)

instance ReadableFrom ExampleTable ExampleData
instance WritableInto ExampleTable ExampleData
instance ReadableFrom ExampleView  ExampleData

exampleDataSample :: ExampleData
exampleDataSample = MkExampleData
  { a1 = toChType (42 :: Int64)
  , a2 = "text"
  , a4 = nilChUUID
  , a3 = 42 
  , a5 = 42
  , a6 = Just "500"
  , a7 = ""
  }








-- >>> showSelect
-- "SELECT a1, a2, a3, a4, a5, a6, a7 FROM \"example\""
showSelect :: StrictByteString
showSelect = renderSelectQuery
  $ operation
    @( Reading ExampleData
    -> ClickHouse ExampleTable
    )

-- >>> showSelectFromView
-- "SELECT a1, a2, a3, a4, a5, a6, a7 FROM \"exampleView\""
showSelectFromView :: StrictByteString
showSelectFromView = renderSelectQuery
  $ operation
    @( Reading ExampleData
    -> ClickHouse ExampleView
    )

-- >>> showInsert
-- "INSERT INTO \"example\" (a1, a2, a3, a4, a5, a6, a7)"
showInsert :: StrictByteString
showInsert = renderInsertHeader
  $ operation
    @( Writing ExampleData
    -> ClickHouse ExampleTable
    )




type SingleFieldTable =
  Table
    "example2"
   '[ Column "a1" ChInt64
    ]
   '[]

newtype SingleFieldRecord = MkSingleFieldRecord {a1 :: Int64}
  deriving (Generic)

instance ReadableFrom SingleFieldTable SingleFieldRecord
instance WritableInto SingleFieldTable SingleFieldRecord
