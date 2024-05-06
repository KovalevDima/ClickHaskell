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
import ClickHouse.DbTypes  (Serializable(..), Deserializable(..), ToChType(..), FromChType(..), IsWriteOptional, IsChType (IsWriteOptional))
import ClickHaskell.Tables (CompiledColumn(..), InterpretableTable(..))


-- GHC included
import Data.ByteString         as BS (split, intercalate, StrictByteString)
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy(..))
import Data.Type.Bool          (If)
import Data.Type.Equality      (type(==))
import Data.Type.Ord           (type(>?), type(<=?))
import Data.Word               (Word8)
import Data.Kind               (Type, Constraint)
import GHC.Generics            (K1(..), M1(..), type (:*:)(..), Rec0, D1, C1, S1, Meta(MetaSel), Generic (..))
import GHC.TypeLits            (KnownSymbol, TypeError, Symbol, ErrorMessage(..), symbolVal)


-- * Writing

class
  ( InterpretableTable table
  , GWritable (GetTableColumns table) (Rep record)
  )
  =>
  WritableInto table record
  where
  default toTsvLine :: (Generic record) => record -> BS.Builder
  toTsvLine :: record -> BS.Builder
  toTsvLine = gToTsvBs @(GetTableColumns table) . from

  default writingColumns :: Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetTableColumns table) @(Rep record)


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
      (IsWriteOptional column)
      (ThereIsNoWriteRequiredColumns columns)
      (TypeError ('Text "Column " :<>: 'Text (GetColumnName column) :<>: 'Text " is required for insert but is missing"))

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
    :$$: 'Text "You can't insert this field"
    )

type family
  (++) (list1 :: [Type]) (list2 :: [Type]) :: [Type]
  where
  (++) '[]            list = list
  (++) (head ': tail) list = tail ++ (head ': list)








-- * Reading

class
  ( InterpretableTable table
  , GReadable (ValidatedTable table) (GetTableColumns table) (Rep record)
  ) =>
  ReadableFrom table record
  where

  default fromTsvLine :: (Generic record) => StrictByteString -> record
  fromTsvLine :: StrictByteString -> record
  fromTsvLine = to . gFromTsvBs @(ValidatedTable table) @(GetTableColumns table)

  default readingColumns :: (Generic record) => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(ValidatedTable table) @(GetTableColumns table) @(Rep record)


class GReadable
  (deivingState :: Maybe ErrorMessage)
  (columns :: [Type])
  f
  where
  gFromTsvBs :: StrictByteString -> f p
  gReadingColumns :: Builder


instance
  ( GReadable
    ('Just
      (    'Text "There is no column \"" :<>: 'Text columnName :<>: 'Text "\" in table."
      :$$: 'Text "You can't select this field"
      )
    )
    '[]
    (S1 (MetaSel (Just columnName) a b f) k)
  ) => GReadable
    'Nothing
    '[]
    (S1 (MetaSel (Just columnName) a b f) k)
  where
  gFromTsvBs _ = error "Unreachable"
  gReadingColumns = error "Unreachable"


instance
  ( TypeError errorMsg
  ) => GReadable ('Just errorMsg) columns genericRep
  where
  gFromTsvBs _ = error "Unreachable"
  gReadingColumns = error "Unreachable"


instance {-# OVERLAPPING #-}
  ( GReadable 'Nothing columns f
  ) => GReadable 'Nothing columns (D1 c (C1 c2 f))
  where
  gFromTsvBs = M1 . M1 . gFromTsvBs @'Nothing @columns
  gReadingColumns = gReadingColumns @'Nothing @columns @f


instance {-# OVERLAPPING #-}
  ( leftCenterTreeElement  ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , derivingState ~ FirstJustOrNothing
    '[ GetGenericProductHeadSelector left `AssumePlacedBefore` leftCenterTreeElement
     , leftCenterTreeElement              `AssumePlacedBefore` rightCenterTreeElement
     , rightCenterTreeElement             `AssumePlacedBefore` GetGenericProductLastSelector right
     ]
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , GReadable derivingState firstColumnsPart left
  , GReadable derivingState secondColumnsPart right
  ) => GReadable 'Nothing columns (left :*: right)
  where
  gFromTsvBs bs = {- [ClickHaskell.DataDsl.ToDo.1]: optimize line deconstruction -}
    let byteStrings = tabSymbol `BS.split` bs
        (leftWords, rightWords) = splitAt (length byteStrings `div` 2) byteStrings
    in  gFromTsvBs @derivingState @firstColumnsPart (BS.intercalate "\t" leftWords)
    :*: gFromTsvBs @derivingState @secondColumnsPart (BS.intercalate "\t" rightWords)
    where
    tabSymbol :: Word8
    tabSymbol = 9
  gReadingColumns
    =          gReadingColumns @derivingState @firstColumnsPart @left
    <> ", " <> gReadingColumns @derivingState @secondColumnsPart @right


instance {-# OVERLAPPING #-}
  ( Deserializable (GetColumnType column)
  , FromChType (GetColumnType column) outputType
  , KnownSymbol columnName
  ) => GReadable
    'Nothing
    (column ': xs)
    (S1 (MetaSel (Just columnName) a b f) (K1 i outputType))
  where
  gFromTsvBs = M1 . K1 . fromChType @(GetColumnType column) @outputType . deserialize
  gReadingColumns = (BS.byteString . BS8.pack . symbolVal) (Proxy @columnName)




type family (sym1 :: Symbol) `AssumePlacedBefore` (sym2 :: Symbol) :: Maybe ErrorMessage
  where
  sym1 `AssumePlacedBefore` sym2 =
    If (sym1 >? sym2)
      ('Just
      (    'Text "Record fields should be sorted alphabetically."
      :$$: 'Text "But field \"" :<>: 'Text sym2 :<>: 'Text "\" placed before \"" :<>: 'Text sym1 :<>: 'Text "\""
      )
      )
      'Nothing

type family GetGenericProductHeadSelector (f :: k -> Type) :: Symbol where
  GetGenericProductHeadSelector (c :*: c2) = GetGenericProductHeadSelector c
  GetGenericProductHeadSelector (D1 _ f)   = GetGenericProductHeadSelector f
  GetGenericProductHeadSelector (C1 _ f)   = GetGenericProductHeadSelector f
  GetGenericProductHeadSelector (S1 (MetaSel (Just sel) _ _ _) _) = sel

type family GetGenericProductLastSelector (f :: k -> Type) :: Symbol where
  GetGenericProductLastSelector (c :*: c2) = GetGenericProductLastSelector c2
  GetGenericProductLastSelector (D1 _ f)   = GetGenericProductLastSelector f
  GetGenericProductLastSelector (C1 _ f)   = GetGenericProductLastSelector f
  GetGenericProductLastSelector (S1 (MetaSel (Just sel) _ _ _) _) = sel

type family SpanByColumnName
  (name :: Symbol)
  (columns :: [Type])
  ::
  ([Type], [Type])
  where
  SpanByColumnName name columns = GoSpanByColumnName name '(columns, '[])

type family GoSpanByColumnName
  (name :: Symbol)
  (acc :: ([Type], [Type]))
  ::
  ([Type], [Type])
  where
  GoSpanByColumnName name '( '[], acc2) = '(Reverse acc2, '[])
  GoSpanByColumnName name '(column ': columns, acc2) =
    If (name <=? GetColumnName column)
      '(Reverse acc2, column ': columns)
      (GoSpanByColumnName name '(columns, column ': acc2))

type family Reverse (b :: [a]) :: [a] where Reverse list = GoReverse list '[]
type family GoReverse (list :: [a]) (acc :: [a])
  where
  GoReverse '[]       acc = acc
  GoReverse (x ': xs) acc = GoReverse xs (x ': acc)

-- | Type family usefull when you have a several posible errors and need to return first one.
-- 
-- @
-- type NotAnError = 'Nothng
-- type Error1     = 'Just ('Text "error1")
-- type Error2     = 'Just ('Text "error2")
--
-- type FirstJustOrNothing '[NotAnError, Error2] = 'Just ('Text "error2")
-- type FirstJustOrNothing '[Error1,     Error2] = 'Just ('Text "error1")
-- type FirstJustOrNothing '[NotAnError]         = 'Nothing
-- @
type family FirstJustOrNothing (a :: [Maybe ErrorMessage]) :: Maybe ErrorMessage
  where
  FirstJustOrNothing '[] = 'Nothing
  FirstJustOrNothing ('Nothing ': xs) = FirstJustOrNothing xs
  FirstJustOrNothing ('Just txt ': xs) = 'Just txt
