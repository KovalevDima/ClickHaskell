module ClickHaskell.Columns where

-- Internal
import ClickHaskell.Primitive

-- GHC included
import Data.Binary.Get
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS8 (pack)
import Data.Traversable (forM)
import Data.Int
import Data.Kind
import Data.Coerce
import Data.Typeable (Proxy (..))
import Data.Bits (Bits ((.&.)))
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)

-- External
import Data.WideWord (Int128 (..))

-- * Column

{- |
Column declaration

For example:

@
type MyColumn = Column "myColumn" ChString
@
-}
data Column (name :: Symbol) (chType :: Type) where
  UInt8Column   :: [UInt8]   -> Column name UInt8
  Int8Column   :: [Int8]   -> Column name Int8
  UInt16Column  :: [UInt16]  -> Column name UInt16
  Int16Column  :: [Int16]  -> Column name Int16
  UInt32Column  :: [UInt32]  -> Column name UInt32
  Int32Column  :: [Int32]  -> Column name Int32
  UInt64Column  :: [UInt64]  -> Column name UInt64
  Int64Column  :: [Int64]  -> Column name Int64
  UInt128Column :: [UInt128] -> Column name UInt128
  Int128Column :: [Int128] -> Column name Int128
  UInt256Column :: [UInt256] -> Column name UInt256
  DateTimeColumn :: [DateTime tz] -> Column name (DateTime tz)
  DateTime64Column :: [DateTime64 precision tz] -> Column name (DateTime64 precision tz)
  DateColumn :: [Date] -> Column name Date
  UUIDColumn :: [UUID] -> Column name UUID
  StringColumn :: [ChString] -> Column name ChString
  ArrayColumn :: [Array chType] -> Column name (Array chType)
  NullableColumn :: [Nullable chType] -> Column name (Nullable chType)
  LowCardinalityColumn :: [chType] -> Column name (LowCardinality chType)

type family GetColumnName column :: Symbol where GetColumnName (Column name columnType) = name
type family GetColumnType column :: Type   where GetColumnType (Column name columnType) = columnType

{-# INLINE [0] columnValues #-}
columnValues :: Column name chType -> [chType]
columnValues column = case column of
  (UInt8Column values) -> values
  (UInt16Column values) -> values
  (UInt32Column values) -> values
  (UInt64Column values) -> values
  (UInt128Column values) -> values
  (UInt256Column values) -> values
  (Int8Column values) -> values
  (Int16Column values) -> values
  (Int32Column values) -> values
  (Int64Column values) -> values
  (Int128Column values) -> values
  (DateColumn values) -> values
  (DateTimeColumn values) -> values
  (DateTime64Column values) -> values;
  (UUIDColumn values) -> values
  (StringColumn values) -> values
  (ArrayColumn values) -> values
  (NullableColumn values) ->  values
  (LowCardinalityColumn values) -> map coerce values

class
  ( IsChType (GetColumnType column)
  , KnownSymbol (GetColumnName column)
  ) =>
  KnownColumn column where
  renderColumnName :: Builder
  renderColumnName = (stringUtf8 . symbolVal @(GetColumnName column)) Proxy

  renderColumnType :: Builder
  renderColumnType = byteString . BS8.pack $ chTypeName @(GetColumnType column)

  mkColumn :: [GetColumnType column] -> Column (GetColumnName column) (GetColumnType column)

instance KnownSymbol name => KnownColumn (Column name UInt8) where
  mkColumn = UInt8Column

instance KnownSymbol name => KnownColumn (Column name UInt16) where
  mkColumn = UInt16Column

instance KnownSymbol name => KnownColumn (Column name UInt32) where
  mkColumn = UInt32Column

instance KnownSymbol name => KnownColumn (Column name UInt64) where
  mkColumn = UInt64Column

instance KnownSymbol name => KnownColumn (Column name UInt128) where
  mkColumn = UInt128Column

instance KnownSymbol name => KnownColumn (Column name UInt256) where
  mkColumn = UInt256Column

instance KnownSymbol name => KnownColumn (Column name Int8)  where
  mkColumn = Int8Column

instance KnownSymbol name => KnownColumn (Column name Int16) where
  mkColumn = Int16Column

instance KnownSymbol name => KnownColumn (Column name Int32) where
  mkColumn = Int32Column

instance KnownSymbol name => KnownColumn (Column name Int64) where
  mkColumn = Int64Column

instance KnownSymbol name => KnownColumn (Column name Int128) where
  mkColumn = Int128Column

instance KnownSymbol name => KnownColumn (Column name Date) where
  mkColumn = DateColumn

instance
  ( KnownSymbol name
  , IsChType (DateTime tz)
  ) =>
  KnownColumn (Column name (DateTime tz))
  where
  mkColumn = DateTimeColumn

instance
  ( KnownSymbol name
  , IsChType (DateTime64 precision tz)
  ) =>
  KnownColumn (Column name (DateTime64 precision tz))
  where
  mkColumn = DateTime64Column

instance KnownSymbol name => KnownColumn (Column name UUID) where
  mkColumn = UUIDColumn

instance
  ( KnownSymbol name
  , IsChType chType
  , IsChType (Nullable chType)
  ) =>
  KnownColumn (Column name (Nullable chType))
  where
  mkColumn = NullableColumn

instance KnownSymbol name => KnownColumn (Column name ChString) where
  mkColumn = StringColumn

instance
  ( KnownSymbol name
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  ) =>
  KnownColumn (Column name (LowCardinality chType))
  where
  mkColumn = LowCardinalityColumn . map coerce

instance KnownSymbol name => KnownColumn (Column name (Array ChString)) where
  mkColumn = ArrayColumn


class SerializableColumn column where
  deserializeColumn :: ProtocolRevision -> UVarInt -> Get column
  serializeColumn :: ProtocolRevision -> column -> Builder

instance
  ( KnownColumn (Column name chType)
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name chType) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows = mkColumn @(Column name chType) <$> replicateGet rev rows

  {-# INLINE serializeColumn #-}
  serializeColumn rev column = mconcat (Prelude.map (serialize @chType rev) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name (Nullable chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows = do
    nulls <- replicateGet @UInt8 rev rows
    mkColumn @(Column name (Nullable chType)) <$>
      forM
        nulls
        (\case
          0 -> Just <$> deserialize @chType rev
          _ -> (Nothing <$ deserialize @chType rev)
        )

  {-# INLINE serializeColumn #-}
  serializeColumn rev column
    =  mconcat (Prelude.map (serialize @UInt8 rev . maybe 1 (const 0)) (columnValues column))
    <> mconcat (Prelude.map (serialize @chType rev . maybe defaultValueOfTypeName id) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (LowCardinality chType))
  , Serializable chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  SerializableColumn (Column name (LowCardinality chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows = do
    _serializationType <- (.&. 0xf) <$> deserialize @UInt64 rev
    _index_size <- deserialize @Int64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    mkColumn @(Column name (LowCardinality chType)) . coerce
      <$> replicateGet @chType rev rows

  {-# INLINE serializeColumn #-}
  serializeColumn _rev (LowCardinalityColumn column) = undefined column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Array chType))
  , Serializable chType
  , TypeError ('Text "Arrays deserialization still unsupported")
  )
  => SerializableColumn (Column name (Array chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev _rows = do
    (arraySize, _offsets) <- readOffsets rev
    _types <- replicateGet @chType rev (fromIntegral arraySize)
    pure $ mkColumn @(Column name (Array chType)) []
    where
    readOffsets :: ProtocolRevision -> Get (UInt64, [UInt64])
    readOffsets revivion = do
      size <- deserialize @UInt64 rev
      (size, ) <$> goArrayOffsets size
      where
      goArrayOffsets arraySize =
        do
        nextOffset <- deserialize @UInt64 revivion
        if arraySize >= nextOffset
          then pure [nextOffset]
          else (nextOffset :) <$> goArrayOffsets arraySize

  {-# INLINE serializeColumn #-}
  serializeColumn _rev _column = undefined
