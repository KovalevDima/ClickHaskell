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
  ( interpretClient
  , initClient, setHttpClientTimeout, HttpChClient, ChCredential(..)
  , Reading, Writing
  )
import ClickHaskell.Generics (WritableInto, ReadableFrom)
import ClickHaskell.Tables
  ( interpretTable
  , Table
  , View, Parameter, mkParameter
  , Column
  )
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


writing :: IO ()
writing = do

  print "1. Initializing client"
  client <- initClient
    @HttpChClient
    exampleCredentials
    Nothing

  print "2. Performing writing"
  interpretClient
    @(Writing ExampleData -> ExampleTable)
    client
    [exampleDataSample]


reading :: IO [ExampleData]
reading = do

  print "1. Initializing client"
  client <- initClient
    @HttpChClient
    exampleCredentials
    (Just $ setHttpClientTimeout 500_000)

  print "2. Performing reading"
  interpretClient
    @(Reading ExampleData -> ExampleTable)
    client


readParametrizedView :: IO [SingleFieldRecord]
readParametrizedView = do

  print "1. Initializing client"
  client <- initClient
    @HttpChClient
    exampleCredentials
    (Just $ setHttpClientTimeout 500_000)

  print "2. Performing reading"
  interpretClient
    @(Reading SingleFieldRecord -> ExampleView)
    client
    (interpretTable @ExampleView (mkParameter "text") (mkParameter "text2"))


exampleCredentials :: ChCredential
exampleCredentials = MkChCredential "default" "" "http://localhost:8123" "default"


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
   '[ Column "totalA1" ChInt64
    ]
   '[ Parameter "param1" ChString
    , Parameter "param2" ChString
    ]

data ExampleData = MkExampleData
  { a1 :: ChInt64
  , a2 :: StrictByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: LowCardinality ChString
  }
  deriving (Generic, Show)

instance ReadableFrom ExampleTable ExampleData
instance WritableInto ExampleTable ExampleData
instance ReadableFrom ExampleView  SingleFieldRecord

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
   '[ Column "totalA1" ChInt64
    ]

newtype SingleFieldRecord = MkSingleFieldRecord
  { totalA1 :: Int64
  }
  deriving (Generic, Show)

instance ReadableFrom SingleFieldTable SingleFieldRecord
instance WritableInto SingleFieldTable SingleFieldRecord
