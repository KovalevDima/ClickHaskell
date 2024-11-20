{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , TypeFamilyDependencies
#-}
{-# LANGUAGE ConstraintKinds #-}

module ClickHaskell.Columns where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.DeSerialization (Serializable (..), DeserializableColumn (..), DeserializableColumns)
import ClickHaskell.Versioning (ProtocolRevision)

-- GHC included
import Data.Binary (Get)
import Data.ByteString.Builder (Builder)
import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

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




-- * To columns generic convertion

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




-- * From columns generic convertion

-- ** Reading

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
