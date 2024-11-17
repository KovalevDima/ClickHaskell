{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , TypeFamilyDependencies
#-}
{-# LANGUAGE ConstraintKinds #-}

module ClickHaskell.Columns where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.DeSerialization (DeserializableColumns, Serializable)

-- GHC included
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal, ErrorMessage(..), Symbol, TypeError, type (+))
import Data.Kind (Type)
import GHC.Generics
import Data.ByteString.Builder (Builder)

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


-- ** Compile time columns size

class KnownNat (ColumnsCount columns) => KnownColumns columns
  where
  type ColumnsCount columns :: Nat
  columnsCount :: UVarInt
  columnsCount = (fromIntegral . natVal) (Proxy @(ColumnsCount columns))

instance KnownColumns (Columns '[])
  where
  type ColumnsCount (Columns '[]) = 0

instance
  KnownNat (1 + ColumnsCount (Columns extraColumns))
  =>
  KnownColumns (Columns (col ': extraColumns))
  where
  type ColumnsCount (Columns (col ': extraColumns)) = 1 + ColumnsCount (Columns extraColumns)


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

type GenericWritable record hasColumns =
  ( Generic record
  , GWritable (GetColumns hasColumns) (Rep record)
  )

class
  ( HasColumns hasColumns
  , KnownColumns (Columns (GetColumns hasColumns))
  , Serializable (Columns (GetColumns hasColumns))
  , DeserializableColumns (Columns (GetColumns hasColumns))
  )
  =>
  WritableInto hasColumns record
  where
  default toColumns :: GenericWritable record hasColumns => UVarInt -> [record] -> Columns (GetColumns hasColumns)
  toColumns :: UVarInt -> [record] -> Columns (GetColumns hasColumns)
  toColumns size = gToColumns @(GetColumns hasColumns) size . map from

  default writingColumns :: GenericWritable record hasColumns => Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns hasColumns) @(Rep record)


class GWritable columns f
  where
  gToColumns :: UVarInt -> [f p] -> Columns columns
  gWritingColumns :: Builder

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gToColumns #-}
  gToColumns size = gToColumns @columns size . map (unM1 . unM1)
  {-# INLINE gWritingColumns #-}
  gWritingColumns = gWritingColumns @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  {-# INLINE gToColumns #-}
  gToColumns size = gToColumns size . map (\((l1 :*: l2) :*: r) -> l1 :*: (l2 :*: r))
  {-# INLINE gWritingColumns #-}
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))

instance
  ( GWritable '[Column name chType] (S1 sel rec)
  , GWritable restColumns right
  )
  =>
  GWritable (Column name chType ': restColumns) (S1 sel rec :*: right)
  where
  {-# INLINE gToColumns #-}
  gToColumns size
    = (\ (a, b)
   -> (case gToColumns @'[Column name chType] size a of
         (AddColumn col Empty) -> AddColumn col $ gToColumns @restColumns size b))
    . unzip . map (\(l :*: r) -> (l, r))

  {-# INLINE gWritingColumns #-}
  gWritingColumns =
    gWritingColumns @'[Column name chType] @(S1 sel rec)
    <> ", " <> gWritingColumns @restColumns @right

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name chType)
  ) =>
  GWritable '[Column name chType] (S1 (MetaSel (Just name) a b f) (Rec0 chType))
  where
  {-# INLINE gToColumns #-}
  gToColumns size rows =
    appendColumn
      ((mkColumn @(Column name chType) size . map (unK1 . unM1)) rows)
      emptyColumns
  {-# INLINE gWritingColumns #-}
  gWritingColumns = renderColumnName @(Column name chType)

instance
  ( ToChType chType inputType
  , KnownColumn (Column name chType)
  ) =>
  GWritable '[Column name chType] (S1 (MetaSel (Just name) a b f) (Rec0 inputType))
  where
  {-# INLINE gToColumns #-}
  gToColumns size rows =
    appendColumn
      ((mkColumn @(Column name chType) size . map (toChType . unK1 . unM1)) rows)
      emptyColumns
  {-# INLINE gWritingColumns #-}
  gWritingColumns = renderColumnName @(Column name chType)




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
  default fromColumns :: GenericReadable record hasColumns => Columns (GetColumns hasColumns) -> [record]
  fromColumns :: Columns (GetColumns hasColumns) -> [record]
  fromColumns = map to . gFromColumns @(GetColumns hasColumns)

  default readingColumns :: GenericReadable record hasColumns => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(GetColumns hasColumns) @(Rep record)


class GReadable columns f
  where
  gFromColumns :: Columns columns -> [f p]
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns = map (M1 . M1) . gFromColumns @columns
  gReadingColumns = gReadingColumns @columns @f

instance
  GReadable columns (left :*: (right1 :*: right2))
  =>
  GReadable columns ((left :*: right1) :*: right2)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev = do
    (l :*: (r1 :*: r2)) <- gFromColumns rev
    pure ((l :*: r1) :*: r2)
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
  gFromColumns (AddColumn column extraColumns) =
    zipWith (:*:)
      (gFromColumns (AddColumn column emptyColumns))
      (gFromColumns extraColumns)
  gReadingColumns =
    renderColumnName @(Column name chType)
    <> ", " <> gReadingColumns @restColumns @right

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name chType)
  ) => GReadable '[Column name chType] ((S1 (MetaSel (Just name) a b f)) (Rec0 chType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns (AddColumn column _) = map (M1 . K1) (columnValues column)
  gReadingColumns = renderColumnName @(Column name chType)

instance
  ( KnownColumn (Column name chType)
  , FromChType chType inputType
  ) => GReadable '[Column name chType] ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns (AddColumn column _) = map (M1 . K1 . fromChType @chType) (columnValues column)
  gReadingColumns = renderColumnName @(Column name chType)
