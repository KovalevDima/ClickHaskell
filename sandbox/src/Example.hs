{-# LANGUAGE
    DataKinds
  , DerivingStrategies
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
import Data.Proxy      (Proxy(..))
import GHC.Generics    (Generic)
import GHC.TypeLits    (SomeSymbol(..), someSymbolVal)


-- 1. Describe table
type ExampleTable =
  Table
    "example"
    '[ DefaultColumn "a1"  ChInt64
     , DefaultColumn "a2"  (LowCardinality ChString)
     , DefaultColumn "a3" ChDateTime
     , DefaultColumn "a4" ChUUID
     , DefaultColumn "a5" ChInt32
     ]
    MergeTree
    '[ OrderBy '["a1"]
     , PartitionBy '["a1"]
     ]


data ExampleData = ExampleData
  { a1 :: Int64
  , a2 :: ByteString
  , a3 :: Word32
  , a4 :: ChUUID
  , a5 :: Int32
  } deriving (Generic)

instance SelectableFrom ExampleTable ExampleData
instance InsertableInto ExampleTable ExampleData


dataExample :: ExampleData
dataExample = ExampleData
  { a1 = 42
  , a2 = "text"
  , a4 = nilChUUID
  , a3 = 42 
  , a5 = 42 :: Int32
  }


-- >>> showCreateExample
-- "CREATE TABLE IF NOT EXISTS example.example (a1 Int64, a2 LowCardinality(String), a3 DateTime, a4 UUID, a5 Int32) Engine=MergeTree PARTITION BY tuple() ORDER BY tuple()"
showCreateExample :: Text
showCreateExample = showCreateTableIfNotExists @(InDatabase "example" ExampleTable)


-- >>> showSelect
-- "SELECT a1,a2,a3,a4,a5 FROM example.example WHERE a2 like '%' FORMAT TSV"
showSelect :: Text
showSelect = case someSymbolVal "" of
  (SomeSymbol (Proxy :: Proxy var)) -> tsvSelectQuery @(("a2" `SuchThat` HasInfix var) ExampleData) @(InDatabase "example"  ExampleTable)
