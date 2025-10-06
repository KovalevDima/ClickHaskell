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
import Debug.Trace (traceWith)

-- * Column

data Columns (columns :: [Type]) where
  Empty :: Columns '[]
  AddColumn
    :: KnownColumn (Column name chType)
    => Column name chType
    -> Columns columns
    -> Columns (Column name chType ': columns)

colLen :: Columns columns -> Int
colLen (AddColumn (col :: col) _) = length $ fromColumn @col col
colLen Empty = 0

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


class
  ( IsChType (GetColumnType column)
  , KnownSymbol (GetColumnName column)
  ) =>
  KnownColumn column where
  renderColumnName :: Builder
  renderColumnName = (stringUtf8 . symbolVal @(GetColumnName column)) Proxy

  renderColumnType :: Builder
  renderColumnType = byteString . BS8.pack $ chTypeName @(GetColumnType column)

  toColumn :: [GetColumnType column] -> Column (GetColumnName column) (GetColumnType column)
  fromColumn :: Column (GetColumnName column) (GetColumnType column) -> [GetColumnType column]

instance KnownSymbol name => KnownColumn (Column name UInt8) where
  toColumn = UInt8Column
  fromColumn (UInt8Column values) = values

instance KnownSymbol name => KnownColumn (Column name UInt16) where
  toColumn = UInt16Column
  fromColumn (UInt16Column values) = values

instance KnownSymbol name => KnownColumn (Column name UInt32) where
  toColumn = UInt32Column
  fromColumn (UInt32Column values) = values

instance KnownSymbol name => KnownColumn (Column name UInt64) where
  toColumn = UInt64Column
  fromColumn (UInt64Column values) = values

instance KnownSymbol name => KnownColumn (Column name UInt128) where
  toColumn = UInt128Column
  fromColumn (UInt128Column values) = values

instance KnownSymbol name => KnownColumn (Column name UInt256) where
  toColumn = UInt256Column
  fromColumn (UInt256Column values) = values

instance KnownSymbol name => KnownColumn (Column name Int8)  where
  toColumn = Int8Column
  fromColumn (Int8Column values) = values

instance KnownSymbol name => KnownColumn (Column name Int16) where
  toColumn = Int16Column
  fromColumn (Int16Column values) = values

instance KnownSymbol name => KnownColumn (Column name Int32) where
  toColumn = Int32Column
  fromColumn (Int32Column values) = values

instance KnownSymbol name => KnownColumn (Column name Int64) where
  toColumn = Int64Column
  fromColumn (Int64Column values) = values

instance KnownSymbol name => KnownColumn (Column name Int128) where
  toColumn = Int128Column
  fromColumn (Int128Column values) = values

instance KnownSymbol name => KnownColumn (Column name Date) where
  toColumn = DateColumn
  fromColumn (DateColumn values) = values

instance
  ( KnownSymbol name
  , IsChType (DateTime tz)
  ) =>
  KnownColumn (Column name (DateTime tz))
  where
  toColumn = DateTimeColumn
  fromColumn (DateTimeColumn values) = values

instance
  ( KnownSymbol name
  , IsChType (DateTime64 precision tz)
  ) =>
  KnownColumn (Column name (DateTime64 precision tz))
  where
  toColumn = DateTime64Column
  fromColumn (DateTime64Column values) = values

instance KnownSymbol name => KnownColumn (Column name UUID) where
  toColumn = UUIDColumn
  fromColumn (UUIDColumn values) = values

instance
  ( KnownSymbol name
  , IsChType chType
  , IsChType (Nullable chType)
  ) =>
  KnownColumn (Column name (Nullable chType))
  where
  toColumn = NullableColumn
  fromColumn (NullableColumn values)  = values

instance KnownSymbol name => KnownColumn (Column name ChString) where
  toColumn = StringColumn
  fromColumn (StringColumn values) = values

instance
  ( KnownSymbol name
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  ) =>
  KnownColumn (Column name (LowCardinality chType))
  where
  toColumn = LowCardinalityColumn . map coerce
  fromColumn (LowCardinalityColumn values) = map coerce values

instance KnownSymbol name => KnownColumn (Column name (Array ChString)) where
  toColumn = ArrayColumn
  fromColumn (ArrayColumn values) = values


class KnownColumn column => SerializableColumn column where
  deserializeColumn :: ProtocolRevision -> UVarInt -> (GetColumnType column -> a) -> Get [a]
  serializeColumn :: ProtocolRevision -> (a -> GetColumnType column) -> [a] -> Builder

instance
  ( KnownColumn (Column name chType)
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name chType) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows f = map f <$> replicateGet rev rows

  {-# INLINE serializeColumn #-}
  serializeColumn rev f column  = mconcat (Prelude.map (serialize @chType rev . f) column)

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name (Nullable chType)) where
  {-# INLINE deserializeColumn #-}

  deserializeColumn rev rows f = do
    nulls <- replicateGet @UInt8 rev rows
    forM
        nulls
        (\case
          0 -> f . Just <$> deserialize @chType rev
          _ -> (f Nothing <$ deserialize @chType rev)
        )

  {-# INLINE serializeColumn #-}
  serializeColumn rev f column
    =  mconcat (Prelude.map (serialize @UInt8 rev . maybe 1 (const 0) . f) column)
    <> mconcat (Prelude.map (serialize @chType rev . maybe defaultValueOfTypeName id . f) column)

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

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Array chType))
  , Serializable chType
  , Show chType
  -- , TypeError ('Text "Arrays deserialization still unsupported")
  )
  => SerializableColumn (Column name (Array chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows _f = do
    !offsets <- traceWith (\offset -> "Offset: " <> show offset) <$> replicateGet @UInt64 rev (traceWith (\rws -> "Rows: " <> show rws) rows)
    let sizes = zipWith (\x1 x2 -> fromIntegral @_ @UVarInt $ x2 - x1) offsets (drop 1 offsets)
    !_values <- traceWith (\value -> "Value: " <> show value) <$> MkChArray . mconcat <$> mapM  (replicateGet @chType rev)
      (traceWith (\size -> "Calculated size: " <> show size) sizes)
    pure []

  {-# INLINE serializeColumn #-}
  serializeColumn _rev _column = undefined
