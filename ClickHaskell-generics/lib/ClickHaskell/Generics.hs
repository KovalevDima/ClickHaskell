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
  ( -- * Generic instances
    WritableInto(..)
  , GWritable(..)

  , ReadableFrom(..)
  , GReadable(..)
  ) where


-- Internal dependencies
import ClickHouse.DbTypes  (Serializable(..), Deserializable(..), ToChType(..), FromChType(..))
import ClickHaskell.Tables (IsColumnDescription(..), TableInterpretable(..))


-- GHC included
import Data.ByteString         as BS (split, intercalate, StrictByteString)
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy(..))
import Data.Type.Bool          (If)
import Data.Type.Ord           (type(>?), type(<=?))
import Data.Word               (Word8)
import Data.Kind               (Type)
import GHC.Generics            (K1(..), M1(..), type (:*:)(..), Rec0, D1, C1, S1, Meta(MetaSel), Generic (..))
import GHC.TypeLits            (KnownSymbol, TypeError, Symbol, ErrorMessage(..), symbolVal)


-- * Writing

class
  ( TableInterpretable table
  ) =>
  WritableInto table record
  where

  default toTsvLine
    ::
    ( GWritable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => record -> BS.Builder
  toTsvLine :: record -> BS.Builder
  toTsvLine = gToTsvBs @(ValidateTable table) @(GetTableColumns table) . from
  {-# NOINLINE toTsvLine #-}

  default renderedWritingColumns
    ::
    ( GWritable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => Builder
  renderedWritingColumns :: Builder
  renderedWritingColumns = gRenderedInsertableColumns @(ValidateTable table) @(GetTableColumns table) @(Rep record)
  {-# NOINLINE renderedWritingColumns #-}


class GWritable
  (deivingState :: Maybe ErrorMessage)
  (columns :: [Type])
  f
  where
  gToTsvBs :: f p -> Builder
  gRenderedInsertableColumns :: Builder


instance
  ( GWritable
    ('Just
      (    'Text "There is no column \"" :<>: 'Text columnName :<>: 'Text "\" in table"
      :$$: 'Text "You can't insert this field"
      )
    )
    '[]
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  ) =>
  GWritable
    'Nothing
    '[]
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  where
  gToTsvBs = error "Unreachable"
  gRenderedInsertableColumns = error "Unreachable"


instance
  ( TypeError errorMsg
  ) => GWritable ('Just errorMsg) columns genericRep
  where
  gToTsvBs _ = error "Unreachable"
  gRenderedInsertableColumns = error "Unreachable"


instance {-# OVERLAPPING #-}
  ( GWritable 'Nothing columns f
  ) => GWritable 'Nothing columns (D1 c f)
  where
  gToTsvBs (M1 re) = gToTsvBs @'Nothing @columns re <> "\n"
  {-# INLINE gToTsvBs #-}

  gRenderedInsertableColumns = gRenderedInsertableColumns @'Nothing @columns @f
  {-# INLINE gRenderedInsertableColumns #-}


instance {-# OVERLAPPING #-}
  ( GWritable 'Nothing columns f
  ) =>
  GWritable 'Nothing columns (C1 c f)
  where
  gToTsvBs (M1 re) = gToTsvBs @'Nothing @columns re
  {-# INLINE gToTsvBs #-}

  gRenderedInsertableColumns = gRenderedInsertableColumns @'Nothing @columns @f
  {-# INLINE gRenderedInsertableColumns #-}


instance {-# OVERLAPPING #-}
  ( leftCenterTreeElement  ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , derivingState ~ FirstJustOrNothing
    '[ GetGenericProductHeadSelector left `AssumePlacedBefore` leftCenterTreeElement
     , leftCenterTreeElement              `AssumePlacedBefore` rightCenterTreeElement
     , rightCenterTreeElement             `AssumePlacedBefore` GetGenericProductLastSelector right
     ]
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , GWritable derivingState firstColumnsPart left
  , GWritable derivingState secondColumnsPart right
  ) => GWritable 'Nothing columns (left :*: right)
  where
  gToTsvBs (left :*: right)
    =          gToTsvBs @derivingState @firstColumnsPart left
    <> "\t" <> gToTsvBs @derivingState @secondColumnsPart right
  {-# INLINE gToTsvBs #-}

  gRenderedInsertableColumns
    =  gRenderedInsertableColumns @derivingState @firstColumnsPart @left
    <> ", "
    <> gRenderedInsertableColumns @derivingState @secondColumnsPart @right
  {-# INLINE gRenderedInsertableColumns #-}


instance {-# OVERLAPPING #-}
  ( Serializable chType
  , ToChType chType inputType
  , columnName ~ GetColumnName column
  , KnownSymbol columnName
  , chType ~ GetColumnType column
  ) => GWritable 'Nothing '[column]
    ( S1 (MetaSel (Just columnName) a b f) (Rec0 inputType)
    )
  where
  gToTsvBs = serialize . toChType @chType @inputType . unK1 . unM1
  {-# INLINE gToTsvBs #-}

  gRenderedInsertableColumns = BS.byteString . BS8.pack $ symbolVal (Proxy @columnName)
  {-# INLINE gRenderedInsertableColumns #-}


instance
  (GWritable
    (If (IsColumnWriteOptional anotherColumn)
      'Nothing
      ('Just
        (    'Text "Column with name "
          :<>: 'Text (GetColumnName anotherColumn)
          :<>: 'Text " is required for insert."
        :$$: 'Text "Add it to your insertable type"
        )
      )
    )
    (column ': moreColumns)
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  , ToChType (GetColumnType column) inputType
  , Serializable (GetColumnType column)
  , KnownSymbol columnName
  )
  => GWritable
    'Nothing
    (column ': anotherColumn ': moreColumns)
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  where
  gToTsvBs = serialize . toChType @(GetColumnType column) @inputType . unK1 . unM1
  {-# INLINE gToTsvBs #-}

  gRenderedInsertableColumns = BS.byteString . BS8.pack $ symbolVal (Proxy @columnName)
  {-# INLINE gRenderedInsertableColumns #-}








-- * Reading

class
  ( TableInterpretable table
  ) =>
  ReadableFrom table record
  where

  default fromTsvLine
    ::
    ( GReadable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => StrictByteString -> record
  fromTsvLine :: StrictByteString -> record
  fromTsvLine = to . gFromTsvBs @(ValidateTable table) @(GetTableColumns table)
  {-# NOINLINE fromTsvLine #-}

  default renderedReadingColumns
    ::
    ( GReadable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => Builder
  renderedReadingColumns :: Builder
  renderedReadingColumns = gRenderedSelectableColumns @(ValidateTable table) @(GetTableColumns table) @(Rep record)
  {-# NOINLINE renderedReadingColumns #-}


class GReadable
  (deivingState :: Maybe ErrorMessage)
  (columns :: [Type])
  f
  where
  gFromTsvBs :: StrictByteString -> f p
  gRenderedSelectableColumns :: Builder


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
  gRenderedSelectableColumns = error "Unreachable"


instance
  ( TypeError errorMsg
  ) => GReadable ('Just errorMsg) columns genericRep
  where
  gFromTsvBs _ = error "Unreachable"
  gRenderedSelectableColumns = error "Unreachable"


instance {-# OVERLAPPING #-}
  ( GReadable 'Nothing columns f
  ) => GReadable 'Nothing columns (D1 c f)
  where
  gFromTsvBs = M1 . gFromTsvBs @'Nothing @columns
  {-# INLINE gFromTsvBs #-}
  gRenderedSelectableColumns = gRenderedSelectableColumns @'Nothing @columns @f
  {-# INLINE gRenderedSelectableColumns #-}


instance {-# OVERLAPPING #-}
  ( GReadable 'Nothing columns f
  ) => GReadable 'Nothing columns (C1 c f)
  where
  gFromTsvBs = M1 . gFromTsvBs @'Nothing @columns
  {-# INLINE gFromTsvBs #-}
  gRenderedSelectableColumns = gRenderedSelectableColumns @'Nothing @columns @f
  {-# INLINE gRenderedSelectableColumns #-}


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
  {-# INLINE gFromTsvBs #-}
  gRenderedSelectableColumns
    =          gRenderedSelectableColumns @derivingState @firstColumnsPart @left
    <> ", " <> gRenderedSelectableColumns @derivingState @secondColumnsPart @right


instance {-# OVERLAPPING #-}
  ( Deserializable chType
  , FromChType chType outputType
  , columnName ~ GetColumnName column
  , KnownSymbol columnName
  , chType ~ GetColumnType column
  ) => GReadable
    'Nothing
    (column ': xs)
    (S1 (MetaSel (Just columnName) a b f) (K1 i outputType))
  where
  gFromTsvBs = M1 . K1 . fromChType @chType @outputType . deserialize
  {-# INLINE gFromTsvBs #-}
  gRenderedSelectableColumns = (BS.byteString . BS8.pack . symbolVal) (Proxy @columnName)
  {-# INLINE gRenderedSelectableColumns #-}








-- * Constants

-- |
-- >>> toEnum @Char tabSymbol
-- '\t'
tabSymbol :: Word8
tabSymbol = 9








-- * Generic helpers

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
