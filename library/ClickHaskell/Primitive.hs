{-# LANGUAGE BangPatterns #-}
module ClickHaskell.Primitive
  ( module ClickHaskell.Primitive.Serialization
  , module ClickHaskell.Primitive.TArray
  , module ClickHaskell.Primitive.TDate
  , module ClickHaskell.Primitive.TDateTime
  , module ClickHaskell.Primitive.TDecimal
  , module ClickHaskell.Primitive.TEnum
  , module ClickHaskell.Primitive.TFloat
  , module ClickHaskell.Primitive.TLowCardinality
  , module ClickHaskell.Primitive.TString
  , module ClickHaskell.Primitive.TNullable
  , module ClickHaskell.Primitive.TUInt
  , module ClickHaskell.Primitive.TUUID

  , ColumnHeader(..), mkHeader, fallbackTypeName
  ) where

-- Internal
import ClickHaskell.Primitive.Serialization
import ClickHaskell.Primitive.TArray 
import ClickHaskell.Primitive.TBool ()
import ClickHaskell.Primitive.TDate (Date)
import ClickHaskell.Primitive.TDateTime (DateTime, DateTime64)
import ClickHaskell.Primitive.TDecimal
import ClickHaskell.Primitive.TEnum (Enum8, Enum16)
import ClickHaskell.Primitive.TFloat (Float32, Float64)
import ClickHaskell.Primitive.TInt ()
import ClickHaskell.Primitive.TLowCardinality
import ClickHaskell.Primitive.TString (ChString)
import ClickHaskell.Primitive.TNullable (Nullable)
import ClickHaskell.Primitive.TUInt
import ClickHaskell.Primitive.TUUID (UUID)

-- Internal
import GHC.Generics
import Data.ByteString (isPrefixOf)

data ColumnHeader = MkColumnHeader
  { name :: ChString
  , type_ :: ChString
  , is_custom :: UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION
  } deriving (Generic, Serializable)

fallbackTypeName :: ProtocolRevision -> ChString -> ChString
fallbackTypeName rev typeName = toChType @ChString $
  if rev < mkRev @DBMS_MIN_REVISION_WITH_TIME_ZONE_PARAMETER_IN_DATETIME_DATA_TYPE
    && isPrefixOf "DateTime(" (fromChType typeName)
  then "DateTime"
  else typeName

mkHeader :: forall column . KnownColumn column => ColumnHeader
mkHeader = let
    name = toChType $ renderColumnName @column
    type_ = toChType $ chTypeName @(GetColumnType column)
    is_custom = AfterRevision 0
    in MkColumnHeader{..}
