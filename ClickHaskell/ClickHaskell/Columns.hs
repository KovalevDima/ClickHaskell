{-# LANGUAGE
    ConstraintKinds
  , OverloadedStrings
  , TemplateHaskell
  , TypeFamilyDependencies
  , TupleSections
  , LambdaCase
#-}

{-# OPTIONS_GHC
  -Wno-orphans
#-}

module ClickHaskell.Columns where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.DeSerialization (Serializable (..), Deserializable(..))
import ClickHaskell.Versioning (ProtocolRevision, SinceRevision, DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION, afterRevision)

-- GHC included
import Control.Monad (forM, replicateM)
import Data.Binary (Get)
import Data.Bits
import Data.ByteString.Builder (Builder, stringUtf8)
import Data.Kind (Type)
import Data.Typeable (Proxy (..))
import Debug.Trace (traceShowId)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)

-- * Columns

emptyColumns :: Columns '[]
emptyColumns = Empty

rowsCount ::  Columns columns -> UVarInt
rowsCount (AddColumn col _) = columnSize col
rowsCount Empty = 0

{-# INLINE [0] appendColumn #-}
appendColumn
  :: KnownColumn (Column name chType)
  => Column name chType
  -> Columns columns
  -> Columns (Column name chType ': columns)
appendColumn = AddColumn


data Columns (columns :: [Type]) where
  Empty :: Columns '[]
  AddColumn
    :: KnownColumn (Column name chType)
    => Column name chType
    -> Columns columns
    -> Columns (Column name chType ': columns)

{- |
Column declaration

For example:

@
type MyColumn = Column "myColumn" ChString
@
-}
data Column (name :: Symbol) (chType :: Type) where
  ChUInt8Column :: UVarInt -> [ChUInt8] -> Column name ChUInt8
  ChUInt16Column :: UVarInt -> [ChUInt16] -> Column name ChUInt16
  ChUInt32Column :: UVarInt -> [ChUInt32] -> Column name ChUInt32
  ChUInt64Column :: UVarInt -> [ChUInt64] -> Column name ChUInt64
  ChUInt128Column :: UVarInt -> [ChUInt128] -> Column name ChUInt128
  ChInt8Column :: UVarInt -> [ChInt8] -> Column name ChInt8
  ChInt16Column :: UVarInt -> [ChInt16] -> Column name ChInt16
  ChInt32Column :: UVarInt -> [ChInt32] -> Column name ChInt32
  ChInt64Column :: UVarInt -> [ChInt64] -> Column name ChInt64
  ChInt128Column :: UVarInt -> [ChInt128] -> Column name ChInt128
  ChDateColumn :: UVarInt -> [ChDate] -> Column name ChDate
  ChDateTimeColumn :: UVarInt -> [ChDateTime] -> Column name ChDateTime
  ChUUIDColumn :: UVarInt -> [ChUUID] -> Column name ChUUID
  ChStringColumn :: UVarInt -> [ChString] -> Column name ChString
  ChArrayColumn :: IsChType chType => UVarInt -> [ChArray chType] -> Column name (ChArray chType)
  NullableColumn :: IsChType chType => UVarInt -> [Nullable chType] -> Column name (Nullable chType)
  LowCardinalityColumn :: (IsLowCardinalitySupported chType, IsChType chType) => UVarInt -> [chType] -> Column name (LowCardinality chType)

type family GetColumnName column :: Symbol
  where
  GetColumnName (Column name columnType) = name

type family GetColumnType column :: Type
  where
  GetColumnType (Column name columnType) = columnType

class
  ( IsChType (GetColumnType column)
  , KnownSymbol (GetColumnName column)
  ) =>
  KnownColumn column where
  renderColumnName :: Builder
  renderColumnName = (stringUtf8 . symbolVal @(GetColumnName column)) Proxy

  renderColumnType :: Builder
  renderColumnType = chTypeName @(GetColumnType column)

  mkColumn :: UVarInt -> [GetColumnType column] -> Column (GetColumnName column) (GetColumnType column)

{-# INLINE [0] columnSize #-}
columnSize :: Column name chType -> UVarInt
columnSize column = case column of
  (ChUInt8Column size _listValues) -> size
  (ChUInt16Column size _listValues) -> size
  (ChUInt32Column size _listValues) -> size
  (ChUInt64Column size _listValues) -> size
  (ChUInt128Column size _listValues) -> size
  (ChInt8Column size _listValues) -> size
  (ChInt16Column size _listValues) -> size
  (ChInt32Column size _listValues) -> size
  (ChInt64Column size _listValues) -> size
  (ChInt128Column size _listValues) -> size
  (ChDateColumn size _nullableValues) -> size
  (ChDateTimeColumn size _nullableValues) -> size
  (ChUUIDColumn size _nullableValues) -> size
  (ChStringColumn size _values) -> size
  (ChArrayColumn size _nullableValues) -> size
  (NullableColumn size _nullableValues) -> size
  (LowCardinalityColumn size _lowCardinalityValues) -> size

{-# INLINE [0] columnValues #-}
columnValues :: Column name chType -> [chType]
columnValues column = case column of
  (ChUInt8Column _size values) -> values
  (ChUInt16Column _size values) -> values
  (ChUInt32Column _size values) -> values
  (ChUInt64Column _size values) -> values
  (ChUInt128Column _size values) -> values
  (ChInt8Column _size values) -> values
  (ChInt16Column _size values) -> values
  (ChInt32Column _size values) -> values
  (ChInt64Column _size values) -> values
  (ChInt128Column _size values) -> values
  (ChDateColumn _size values) -> values
  (ChDateTimeColumn _size values) -> values
  (ChUUIDColumn _size values) -> values
  (ChStringColumn _size values) -> values
  (ChArrayColumn _size arrayValues) -> arrayValues
  (NullableColumn _size nullableValues) ->  nullableValues
  (LowCardinalityColumn _size lowCardinalityValues) -> map fromChType lowCardinalityValues

instance KnownSymbol name => KnownColumn (Column name ChUInt8) where mkColumn = ChUInt8Column
instance KnownSymbol name => KnownColumn (Column name ChUInt16) where mkColumn = ChUInt16Column
instance KnownSymbol name => KnownColumn (Column name ChUInt32) where mkColumn = ChUInt32Column
instance KnownSymbol name => KnownColumn (Column name ChUInt64) where mkColumn = ChUInt64Column
instance KnownSymbol name => KnownColumn (Column name ChUInt128) where mkColumn = ChUInt128Column
instance KnownSymbol name => KnownColumn (Column name ChInt8)  where mkColumn = ChInt8Column
instance KnownSymbol name => KnownColumn (Column name ChInt16) where mkColumn = ChInt16Column
instance KnownSymbol name => KnownColumn (Column name ChInt32) where mkColumn = ChInt32Column
instance KnownSymbol name => KnownColumn (Column name ChInt64) where mkColumn = ChInt64Column
instance KnownSymbol name => KnownColumn (Column name ChInt128) where mkColumn = ChInt128Column
instance KnownSymbol name => KnownColumn (Column name ChDate) where mkColumn = ChDateColumn
instance KnownSymbol name => KnownColumn (Column name ChDateTime) where mkColumn = ChDateTimeColumn
instance KnownSymbol name => KnownColumn (Column name ChUUID) where mkColumn = ChUUIDColumn
instance
  ( KnownSymbol name
  , IsChType chType
  , IsChType (Nullable chType)
  ) =>
  KnownColumn (Column name (Nullable chType)) where mkColumn = NullableColumn
instance KnownSymbol name => KnownColumn (Column name ChString) where mkColumn = ChStringColumn
instance
  ( KnownSymbol name
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  ) =>
  KnownColumn (Column name (LowCardinality chType)) where mkColumn size = LowCardinalityColumn size . map fromChType
instance KnownSymbol name => KnownColumn (Column name (ChArray ChString)) where mkColumn = ChArrayColumn


-- ** Columns

instance
  Serializable (Columns '[])
  where
  {-# INLINE serialize #-}
  serialize _rev Empty = ""

instance
  ( Serializable (Columns columns)
  , Serializable col
  ) =>
  Serializable (Columns (col ': columns))
  where
  {-# INLINE serialize #-}
  serialize rev (AddColumn col columns) = serialize rev col <> serialize rev columns

instance
  ( KnownColumn (Column name chType)
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name chType) where
  {-# INLINE serialize #-}
  serialize rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name (Nullable chType)) where
  {-# INLINE serialize #-}
  serialize rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    -- Nulls
    <> mconcat (Prelude.map (serialize @ChUInt8 rev . maybe 1 (const 0)) (columnValues column))
    -- Values
    <> mconcat (Prelude.map (serialize @chType rev . maybe defaultValueOfTypeName id) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , IsChType chType
  , Serializable chType
  , TypeError ('Text "LowCardinality serialization still unsupported")
  ) => Serializable (Column name (LowCardinality chType)) where
  {-# INLINE serialize #-}
  serialize rev (LowCardinalityColumn _ column)
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> undefined column










-- * (De)serialization

-- ** Serialization

-- *** Generic API

type GenericWritable record columns =
  ( Generic record
  , GWritable columns (Rep record)
  )

class
  ( HasColumns (Columns (GetColumns columns))
  , Serializable (Columns (GetColumns columns))
  , DeserializableColumns (Columns (GetColumns columns))
  ) =>
  WritableInto columns record
  where
  default serializeRecords :: GenericWritable record (GetColumns columns) => ProtocolRevision -> UVarInt -> [record] -> Builder
  serializeRecords :: ProtocolRevision -> UVarInt -> [record] -> Builder
  serializeRecords rev size = gSerializeRecords @(GetColumns columns) rev size . map from

  default writingColumns :: GenericWritable record (GetColumns columns) => Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns columns) @(Rep record)

  default columnsCount :: GenericWritable record (GetColumns columns) => UVarInt
  columnsCount :: UVarInt
  columnsCount = gColumnsCount @(GetColumns columns) @(Rep record)

class GWritable (columns :: [Type]) f
  where
  gSerializeRecords :: ProtocolRevision -> UVarInt -> [f p] -> Builder
  gWritingColumns :: Builder
  gColumnsCount :: UVarInt

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev size = gSerializeRecords @columns rev size . map (unM1 . unM1)
  {-# INLINE gWritingColumns #-}
  gWritingColumns = gWritingColumns @columns @f
  gColumnsCount = gColumnsCount @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev size = gSerializeRecords @columns rev size . map (\((l1 :*: l2) :*: r) -> l1 :*: (l2 :*: r))
  {-# INLINE gWritingColumns #-}
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))
  gColumnsCount = gColumnsCount @columns @(left1 :*: (left2 :*: right))

instance
  ( GWritable '[Column name chType] (S1 (MetaSel (Just name) a b f) rec)
  , GWritable restColumns right
  , '(Column name chType, restColumns)~ TakeColumn name columns
  )
  =>
  GWritable columns (S1 (MetaSel (Just name) a b f) rec :*: right)
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev size
    = (\(a, b) -> gSerializeRecords @'[Column name chType] rev size a <> gSerializeRecords @restColumns rev size b)
    . unzip . map (\(l :*: r) -> (l, r))

  {-# INLINE gWritingColumns #-}
  gWritingColumns =
    gWritingColumns @'[Column name chType] @(S1 (MetaSel (Just name) a b f) rec)
    <> ", " <> gWritingColumns @restColumns @right
  gColumnsCount = gColumnsCount @'[Column name chType] @(S1 (MetaSel (Just name) a b f) rec) + gColumnsCount @restColumns @right

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name chType)
  , ToChType chType inputType
  , Serializable (Column name chType)
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  ) =>
  GWritable columns (S1 (MetaSel (Just name) a b f) (Rec0 inputType))
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev size = serialize rev . mkColumn @(Column name chType) size . map (toChType . unK1 . unM1)
  {-# INLINE gWritingColumns #-}
  gWritingColumns = renderColumnName @(Column name chType)
  gColumnsCount = 1




-- ** Deserialization

-- *** Generic API

type GenericReadable record hasColumns =
  ( Generic record
  , GReadable (GetColumns hasColumns) (Rep record)
  )

class
  ( HasColumns hasColumns
  , DeserializableColumns (Columns (GetColumns hasColumns))
  ) =>
  ReadableFrom hasColumns record
  where
  default deserializeColumns :: GenericReadable record hasColumns => ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns :: ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns rev size = map to <$> gFromColumns @(GetColumns hasColumns) rev size

  default readingColumns :: GenericReadable record hasColumns => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(GetColumns hasColumns) @(Rep record)


class GReadable (columns :: [Type]) f
  where
  gFromColumns :: ProtocolRevision -> UVarInt -> Get [f p]
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = map (M1 . M1) <$> gFromColumns @columns rev size
  gReadingColumns = gReadingColumns @columns @f

instance
  GReadable columns (left :*: (right1 :*: right2))
  =>
  GReadable columns ((left :*: right1) :*: right2)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = do
    list <- gFromColumns @columns rev size
    pure [(l :*: r1) :*: r2 | (l :*: (r1 :*: r2)) <- list]
  gReadingColumns = gReadingColumns @columns @(left :*: (right1 :*: right2))


instance
  ( KnownColumn (Column name chType)
  , GReadable '[Column name chType] (S1 sel rec)
  , GReadable restColumns right
  )
  =>
  GReadable
    (Column name chType ': restColumns)
    (S1 sel rec :*: right)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = do
    zipWith (:*:)
      <$> gFromColumns @'[Column name chType] rev size
      <*> gFromColumns @restColumns rev size
  gReadingColumns =
    renderColumnName @(Column name chType)
    <> ", " <> gReadingColumns @restColumns @right

instance
  ( KnownColumn (Column name chType)
  , DeserializableColumn (Column name chType)
  , FromChType chType inputType
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  ) => GReadable columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = map (M1 . K1 . fromChType @chType) . columnValues <$> deserializeColumn @(Column name chType) rev size
  gReadingColumns = renderColumnName @(Column name chType)

-- ***

-- ** Raw columns deserialization

class DeserializableColumns columns where
  deserializeRawColumns :: ProtocolRevision -> UVarInt -> Get columns

instance
  DeserializableColumns (Columns '[])
  where
  {-# INLINE deserializeRawColumns #-}
  deserializeRawColumns _rev _rows = pure Empty

instance 
  ( KnownColumn (Column name chType)
  , DeserializableColumn (Column name chType)
  , DeserializableColumns (Columns extraColumns)
  )
  =>
  DeserializableColumns (Columns (Column name chType ': extraColumns))
  where
  {-# INLINE deserializeRawColumns #-}
  deserializeRawColumns rev rows =
    AddColumn
      <$> deserializeColumn rev rows
      <*> deserializeRawColumns @(Columns extraColumns) rev rows


-- ** Column deserialization

{-# SPECIALIZE replicateM :: Int -> Get chType -> Get [chType] #-}

class DeserializableColumn column where
  deserializeColumn :: ProtocolRevision -> UVarInt -> Get column

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name chType) where
  deserializeColumn rev rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    column <- replicateM (fromIntegral rows) (deserialize @chType rev)
    pure $ mkColumn @(Column name chType) rows column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name (Nullable chType)) where
  deserializeColumn rev rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    nulls <- replicateM (fromIntegral rows) (deserialize @ChUInt8 rev)
    nullable <-
      forM
        nulls
        (\case
          0 -> Just <$> deserialize @chType rev
          _ -> (Nothing <$ deserialize @chType rev)
        )
    pure $ mkColumn @(Column name (Nullable chType)) rows nullable

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (LowCardinality chType))
  , Deserializable chType
  , ToChType (LowCardinality chType) chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  DeserializableColumn (Column name (LowCardinality chType)) where
  deserializeColumn rev rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    _serializationType <- (.&. 0xf) <$> deserialize @ChUInt64 rev
    _index_size <- deserialize @ChInt64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    lc <- replicateM (fromIntegral rows) (toChType <$> deserialize @chType rev)
    pure $ mkColumn @(Column name (LowCardinality chType)) rows lc

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (ChArray chType))
  , Deserializable chType
  , TypeError ('Text "Arrays deserialization still unsupported")
  )
  => DeserializableColumn (Column name (ChArray chType)) where
  deserializeColumn rev rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    (arraySize, _offsets) <- traceShowId <$> readOffsets rev
    _types <- replicateM (fromIntegral arraySize) (deserialize @chType rev)
    pure $ mkColumn @(Column name (ChArray chType)) rows []
    where
    readOffsets :: ProtocolRevision -> Get (ChUInt64, [ChUInt64])
    readOffsets revivion = do
      size <- deserialize @ChUInt64 rev
      (size, ) <$> go size
      where
      go arraySize =
        do
        nextOffset <- deserialize @ChUInt64 revivion
        if arraySize >= nextOffset
          then pure [nextOffset]
          else (nextOffset :) <$> go arraySize



-- ** Columns extraction helper

class
  HasColumns hasColumns
  where
  type GetColumns hasColumns :: [Type]

instance HasColumns (Columns columns)
  where
  type GetColumns (Columns columns) = columns


-- ** Take column by name from list of columns

type family
  TakeColumn (name :: Symbol) (columns :: [Type]) :: (Type, [Type])
  where
  TakeColumn name columns = GoTakeColumn name columns '[]

type family
  GoTakeColumn name (columns :: [Type]) (acc :: [Type]) :: (Type, [Type])
  where
  GoTakeColumn name (Column name chType ': columns) acc = '(Column name chType, acc ++ columns)
  GoTakeColumn name (Column name1 chType ': columns) acc = (GoTakeColumn name columns (Column name1 chType ': acc))
  GoTakeColumn name '[]                 acc = TypeError
    (    'Text "There is no column \"" :<>: 'Text name :<>: 'Text "\" in table"
    :$$: 'Text "You can't use this field"
    )

type family
  (++) (list1 :: [Type]) (list2 :: [Type]) :: [Type]
  where
  (++) '[]            list = list
  (++) (head ': tail) list = tail ++ (head ': list)