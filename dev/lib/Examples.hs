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


-- Internal
import ClickHaskell.Client
  ( interpretClient, ChResponse
  , initClient, setHttpClientTimeout, HttpChClient, ChCredential(..)
  , Reading, Writing 
  )
import ClickHaskell.Generics (WritableInto, ReadableFrom)
import ClickHaskell.Tables   (Table, Column, View)
import ClickHouse.DbTypes
  ( toChType
  , ChUUID, ChDateTime, ChInt32, ChInt64, ChString
  , LowCardinality, Nullable
  )


-- GHC included
import Data.ByteString (StrictByteString)
import Data.Int        (Int32, Int64)
import Data.Word       (Word32, Word64)
import GHC.Generics    (Generic)


write :: IO (ChResponse ())
write = do

  print "1. Initializing client"
  client <- initClient
    @HttpChClient
    exampleCredentials
    Nothing

  print "2. Performing client"
  interpretClient
    @(Writing ExampleData -> ExampleTable)
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
  interpretClient
    @(Reading ExampleData -> ExampleTable)
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
   '[
    ]

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
  , a4 = toChType (0 :: Word64)
  , a3 = 42 
  , a5 = 42
  , a6 = Just "500"
  , a7 = ""
  }

type SingleFieldTable =
  Table
    "example2"
   '[ Column "a1" ChInt64
    ]

newtype SingleFieldRecord = MkSingleFieldRecord {a1 :: Int64}
  deriving (Generic)

instance ReadableFrom SingleFieldTable SingleFieldRecord
instance WritableInto SingleFieldTable SingleFieldRecord
