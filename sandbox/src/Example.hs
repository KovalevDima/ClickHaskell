{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DeriveAnyClass
  , TypeApplications
  , TypeOperators
  , ScopedTypeVariables
#-}

module Example where

-- Internal dependencies
import ClickHaskell

-- GHC included libraries imports
import Data.Text (Text)


-- 1. Describe table
type ExampleTable =
  Table
    "example"
    '[ DefaultColumn "string"   (LowCardinality ChString)
     , DefaultColumn "int64"    ChInt64
     , DefaultColumn "dateTime" ChDateTime
     , DefaultColumn "uuid"     ChUUID
     ]
    MergeTree
    '["string", "int64"]
    '["string"]

-- 2. Separate data you will work with
data ExampleData = ExampleData
  { string   :: (LowCardinality ChString)
  , int64    :: ChInt64
  , dateTime :: ChDateTime
  , uuid     :: ChUUID
  }
  deriving (Generic, HasChSchema, Show)




-- >>> showCreateExample
-- "CREATE TABLE IF NOT EXISTS example.example (string LowCardinality(String), int64 Int64, dateTime DateTime, uuid UUID) Engine=MergeTree PARTITION BY (string, int64) ORDER BY (string)"
showCreateExample :: String
showCreateExample = showCreateTableIfNotExists @(InDatabase "example" ExampleTable)


-- >>> showSelect
-- "SELECT string,int64,dateTime,uuid FROM example.example WHERE fieldName=='mysymbol' FORMAT TSV"
showSelect :: Text
showSelect = case someSymbolVal "mysymbol" of (SomeSymbol (Proxy :: Proxy var)) -> tsvSelectQuery @(("fieldName" `SampledBy` EqualityWith var) ExampleData) @(InDatabase "example" ExampleTable)
