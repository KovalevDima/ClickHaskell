module ClickHaskell.Primitive.TLowCardinality where

-- Internal
import ClickHaskell.Primitive.Serialization
import ClickHaskell.Primitive.TString
import ClickHaskell.Primitive.TNullable
import ClickHaskell.Primitive.TInt
import ClickHaskell.Primitive.TUInt

-- GHC included
import Control.DeepSeq (NFData)
import Data.Bits ((.&.))
import Data.Coerce (coerce)
import Data.String (IsString (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)




-- ** LowCardinality

-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
instance IsLowCardinalitySupported chType => IsChType (LowCardinality chType)
  where
  chTypeName = "LowCardinality(" <> chTypeName @chType <> ")"
  defaultValueOfTypeName = MkLowCardinality $ defaultValueOfTypeName @chType

deriving newtype instance (Eq chType, IsLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving newtype instance (NFData chType, IsLowCardinalitySupported chType) => NFData (LowCardinality chType)
deriving newtype instance IsString (LowCardinality ChString)

class IsChType chType => IsLowCardinalitySupported chType
instance IsLowCardinalitySupported ChString
instance
  ( IsLowCardinalitySupported chType
  , IsChType (Nullable chType)
  ) =>
  IsLowCardinalitySupported (Nullable chType)

instance {-# OVERLAPPABLE #-}
  ( IsChType chType
  , TypeError
    (    'Text "LowCardinality("  ':<>: 'ShowType chType  ':<>: 'Text ") is unsupported"
    ':$$: 'Text "Use one of these types:"
    ':$$: 'Text "  ChString"
    ':$$: 'Text "  DateTime"
    ':$$: 'Text "  Nullable(T)"
    )
  ) => IsLowCardinalitySupported chType

instance
  ToChType inputType chType
  =>
  ToChType (LowCardinality inputType) chType where
  toChType = MkLowCardinality . toChType
  fromChType (MkLowCardinality lc)= fromChType @inputType lc

instance ToQueryPart chType => ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (LowCardinality chType))
  , Serializable chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  SerializableColumn (Column name (LowCardinality chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows f = do
    _serializationType <- (.&. 0xf) <$> deserialize @UInt64 rev
    _index_size <- deserialize @Int64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    map f . coerce
      <$> replicateGet @chType rev rows

  {-# INLINE serializeColumn #-}
  serializeColumn _rev column = undefined column
