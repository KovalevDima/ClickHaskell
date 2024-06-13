{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module ClickHaskell.Generics
(
-- * Generic instances
  WritableInto(..)
, GWritable(..)

, ReadableFrom(..)
, GReadable(..)
) where


-- Internal dependencies
import ClickHouse.DbTypes  (Serializable(..), Deserializable(..), ToChType(..), FromChType(..))
import ClickHaskell.Tables (CompiledColumn(..), InterpretableTable(..), HasColumns(..))


-- GHC included
import Data.ByteString         as BS (StrictByteString, drop)
import Data.ByteString.Char8   as BS8 (span)
import Data.ByteString.Builder as BS (Builder)
import Data.Type.Bool          (If)
import Data.Type.Equality      (type(==))
import Data.Kind               (Type, Constraint)
import GHC.Generics            (K1(..), M1(..), type (:*:)(..), Rec0, D1, C1, S1, Meta(MetaSel), Generic (..))
import GHC.TypeLits            (TypeError, Symbol, ErrorMessage(..))


-- * Writing

class
  ( InterpretableTable table
  , GWritable (GetColumns table) (Rep record)
  )
  =>
  WritableInto table record
  where
  default toTsvLine :: (Generic record) => record -> BS.Builder
  toTsvLine :: record -> BS.Builder
  toTsvLine = gToTsvBs @(GetColumns table) . from

  default writingColumns :: Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns table) @(Rep record)


class GWritable
  (columns :: [Type])
  f
  where
  gToTsvBs :: f p -> Builder
  gWritingColumns :: Builder

instance {-# OVERLAPPING #-}
  ( GWritable columns f
  )
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  gToTsvBs (M1 (M1 re)) = gToTsvBs @columns re <> "\n"
  gWritingColumns = gWritingColumns @columns @f

instance {-# OVERLAPPING #-}
  ( GWritable columns (left1 :*: (left2 :*: right))
  )
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  gToTsvBs ((left1 :*: left2) :*: right) = gToTsvBs @columns (left1 :*: (left2 :*: right))
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))

instance
  ( Serializable (GetColumnType column)
  , ToChType (GetColumnType column) inputType
  , CompiledColumn column
  , GWritable restColumns right
  , GWritable '[column] ((S1 (MetaSel (Just typeName) a b f)) (Rec0 inputType))
  , '(column, restColumns) ~ TakeColumn typeName columns
  )
  =>
  GWritable columns ((S1 (MetaSel (Just typeName) a b f)) (Rec0 inputType) :*: right)
  where
  gToTsvBs (M1 (K1 dataType) :*: right)
    =  (serialize . toChType @(GetColumnType column)) dataType
    <> "\t"
    <> gToTsvBs @restColumns right
  gWritingColumns = renderColumnName @column <> ", " <> gWritingColumns @restColumns @right

instance
  ( ThereIsNoWriteRequiredColumns restColumns
  , Serializable (GetColumnType column)
  , ToChType (GetColumnType column) inputType
  , CompiledColumn column
  , '(column, restColumns) ~ TakeColumn typeName columns
  ) =>
  GWritable columns (S1 (MetaSel (Just typeName) a b f) (Rec0 inputType))
  where
  gToTsvBs = serialize . toChType @(GetColumnType column) @inputType . unK1 . unM1
  gWritingColumns = renderColumnName @column


type family ThereIsNoWriteRequiredColumns (columns :: [Type]) :: Constraint where
  ThereIsNoWriteRequiredColumns '[] = ()
  ThereIsNoWriteRequiredColumns (column ': columns) =
    If
      (WriteOptionalColumn column)
      (ThereIsNoWriteRequiredColumns columns)
      (TypeError ('Text "Column " :<>: 'Text (GetColumnName column) :<>: 'Text " is required for insert but is missing"))








-- * Reading

class
  ( HasColumns table
  , GReadable (GetColumns table) (Rep record)
  ) =>
  ReadableFrom table record
  where

  default fromTsvLine :: (Generic record) => StrictByteString -> record
  fromTsvLine :: StrictByteString -> record
  fromTsvLine = to . gFromTsvBs @(GetColumns table)

  default readingColumns :: (Generic record) => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(GetColumns table) @(Rep record)

class GReadable
  (columns :: [Type])
  f
  where
  gFromTsvBs :: StrictByteString -> f p
  gReadingColumns :: Builder

instance {-# OVERLAPPING #-}
  ( GReadable columns f
  ) => GReadable columns (D1 c (C1 c2 f))
  where
  gFromTsvBs = M1 . M1 . gFromTsvBs @columns
  gReadingColumns = gReadingColumns @columns @f


instance {-# OVERLAPPING #-}
  ( GReadable columns (left1 :*: (left2 :*: right))
  ) => GReadable columns ((left1 :*: left2) :*: right)
  where
  gFromTsvBs bs =
    let (left1 :*: (left2 :*: right)) = gFromTsvBs @columns bs
    in ((left1 :*: left2) :*: right)
  gReadingColumns = gReadingColumns @columns @(left1 :*: (left2 :*: right))

instance {-# OVERLAPPING #-}
  ( CompiledColumn column
  , '(column, restColumns) ~ TakeColumn selectorName columns
  , FromChType (GetColumnType column) inputType
  , Deserializable (GetColumnType column)
  , GReadable restColumns right
  ) => GReadable columns (S1 (MetaSel (Just selectorName) a b f) (Rec0 inputType) :*: right)
  where
  gFromTsvBs bs = 
    let (beforeTab, afterTab) = BS8.span (/= '\t') bs 
    in 
    (M1 . K1 . fromChType @(GetColumnType column) . deserialize $ beforeTab) :*: gFromTsvBs @restColumns @right (BS.drop 1 afterTab)
  gReadingColumns = renderColumnName @column <> ", " <> gReadingColumns @restColumns @right

instance {-# OVERLAPPING #-}
  ( CompiledColumn column
  , '(column, restColumns) ~ TakeColumn selectorName columns
  , Deserializable (GetColumnType column)
  , FromChType (GetColumnType column) inputType
  ) => GReadable columns ((S1 (MetaSel (Just selectorName) a b f)) (Rec0 inputType))
  where
  gFromTsvBs = M1 . K1 . fromChType @(GetColumnType column) . deserialize
  gReadingColumns = renderColumnName @column




-- * Reusable in both scenarios

type family
  TakeColumn (name :: Symbol) (columns :: [Type]) :: (Type, [Type])
  where
  TakeColumn name columns = GoTakeColumn name columns '[]

type family
  GoTakeColumn name (columns :: [Type]) (acc :: [Type]) :: (Type, [Type])
  where
  GoTakeColumn name (column ': columns) acc = If (name == GetColumnName column) '(column, acc ++ columns) (GoTakeColumn name columns (column ': acc))
  GoTakeColumn name '[]                 acc = TypeError
    (    'Text "There is no column \"" :<>: 'Text name :<>: 'Text "\" in table"
    :$$: 'Text "You can't use this field"
    )

type family
  (++) (list1 :: [Type]) (list2 :: [Type]) :: [Type]
  where
  (++) '[]            list = list
  (++) (head ': tail) list = tail ++ (head ': list)
