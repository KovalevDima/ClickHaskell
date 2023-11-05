{-# LANGUAGE
    DataKinds
  , DerivingStrategies
  , DuplicateRecordFields
  , OverloadedStrings
  , UndecidableInstances
#-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Example where

-- Internal dependencies
import ClickHaskell

-- GHC included libraries imports
import Data.ByteString (ByteString)
import Data.Int        (Int32)
import GHC.Generics    (Generic)


-- 1. Describe table
type ExampleTable =
  Table
    "example"
    '[ DefaultColumn "a1" ChInt64
     , DefaultColumn "a2" (LowCardinality ChString)
     , DefaultColumn "a3" ChDateTime
     , DefaultColumn "a4" ChUUID
     , DefaultColumn "a5" ChInt32
     , DefaultColumn "a6" (Nullable ChString)
     , DefaultColumn "a7" (LowCardinality ChString) 
     ]
    '[ ExpectsFiltrationBy '["a1"]
     ]


type SingleFieldTable =
  Table
    "example2"
    '[ DefaultColumn "a1" ChInt64
     ]
    '[
     ]


newtype SingleFieldRecord = MkSingleFieldRecord {a1 :: Int64} deriving Generic
instance SelectableFrom SingleFieldTable SingleFieldRecord
instance InsertableInto SingleFieldTable SingleFieldRecord


data ExampleData = ExampleData
  { a1 :: ChInt64
  , a2 :: ByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  , a6 :: Nullable ChString
  , a7 :: LowCardinality ChString
  } deriving (Generic)

instance SelectableFrom ExampleTable ExampleData
instance InsertableInto ExampleTable ExampleData


dataExample :: ExampleData
dataExample = ExampleData
  { a1 = toCh @Int64 42
  , a2 = "text"
  , a4 = nilChUUID
  , a3 = 42 
  , a5 = 42
  , a6 = Just "500"
  , a7 = toCh @Text "5"
  }


-- >>> showSelect
-- "SELECT a1,a2,a3,a4,a5,a6,a7 FROM example.example WHERE a3=0000000042 AND a2='text' FORMAT TSV"
showSelect :: ByteString
showSelect = renderSelectQuery
  $ constructSelection
    @(InDatabase "example" ExampleTable)
    @(Result ExampleData
      %% EqualTo "a2" Variable
      %% EqualTo "a3" Variable
    )
    (toCh @Word32 42)
    (toCh @ByteString "text")


-- >>> showSelect2
-- "SELECT a1,a2,a3,a4,a5,a6,a7 FROM example.example FORMAT TSV"
showSelect2 :: ByteString
showSelect2 = renderSelectQuery
  $ constructSelection
    @(InDatabase "example" ExampleTable)
    @(Result ExampleData
    )
