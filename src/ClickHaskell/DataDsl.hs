{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  , UndecidableSuperClasses
#-}
{-# LANGUAGE InstanceSigs #-}
module ClickHaskell.DataDsl
  (
  -- * Inserting
  InsertableInto(toTsvLine)

  , tsvInsertQueryHeader

  -- * Seleting
  , SelectableFrom(fromTsvLine)
  , renderSelectQuery

  , SelectionDescription
  , IsSelectionDescription(SelectionDescriptionConstructor, ToSelectionResult, GetFilters)
  , constructSelection
  -- , setDbName

  , type (%%)
  , Result
  , EqualTo, HasInfix
  , Variable
  ) where

-- Internal dependencies
import ClickHaskell.TableDsl   (IsTable(..), IsColumnDescription (..))
import ClickHaskell.DbTypes    (Serializable(serialize), Deserializable(deserialize), FromChType(fromChType), ToChType(toChType), QuerySerializable (renderForQuery))


-- GHC included libraries imports
import Data.ByteString    as BS (ByteString, intercalate, split)
import Data.Data          (Proxy(..))
import Data.Kind          (Type)
import Data.Text          as T (Text, intercalate, pack)
import Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import Data.Type.Bool     (If)
import Data.Type.Equality (type(==))
import Data.Type.Ord      (type(>?), type(<=?))
import Data.String        (IsString(..))
import GHC.Generics       (Generic(..), K1(..), M1(..), type (:*:)(..), D1, C1, S1, Meta(MetaSel), Rec0)
import GHC.TypeError      (TypeError, ErrorMessage(..))
import GHC.TypeLits       (Symbol, KnownSymbol, symbolVal)


-- * Inserting


class
  ( IsTable table
  ) => InsertableInto table record where
  default toTsvLine
    ::
    ( GInsertable
      (TableValidationResult table)
      (GetTableColumns table)
      (Rep record)  
    , Generic record
    ) => record -> BS.ByteString

  toTsvLine :: record -> BS.ByteString
  toTsvLine
    = gToTsvBs
      @(TableValidationResult table)
      @(GetTableColumns table)
    . from
  {-# NOINLINE toTsvLine #-}


instance  {-# OVERLAPPABLE #-}
  ( IsTable table
  , Generic record
  , TypeError
    (    'Text "You didn't provide"
    :$$: 'Text "  ( InsertableInto "
    :$$: 'Text "    (Table \"" :<>: 'Text (GetTableName table) :<>: 'Text "\" ...) "
    :$$: 'Text "    (" :<>: ShowType record :<>: 'Text ")"
    :$$: 'Text "  )"
    :$$: 'Text "instance"
    :$$: 'Text "Derive it via:"
    :$$: 'Text "  |data " :<>: ShowType record
    :$$: 'Text "  |  { .."
    :$$: 'Text "  |  } deriving (Generic)"
    :$$: 'Text "  |instance InsertableInto (Table \"" :<>: 'Text (GetTableName table)  :<>: 'Text "\" ...) " :<>: ShowType record
    )
  ) => InsertableInto table record
  where
  toTsvLine = error "Unreachable"




tsvInsertQueryHeader :: forall locatedTable description .
  ( InsertableInto locatedTable description
  ) => Text
tsvInsertQueryHeader =
  let columnsMapping = T.intercalate "," $ getTableRenderedColumnsNames @locatedTable
  in "INSERT INTO " <> getTableName @locatedTable
  <> " (" <> columnsMapping <> ")"
  <> " FORMAT TSV\n"




class GInsertable
  (deivingState :: Maybe ErrorMessage)
  (columns :: [Type])
  f
  where
  gToTsvBs :: f p -> BS.ByteString


instance
  ( TypeError errorMsg
  ) => GInsertable ('Just errorMsg) columns genericRep
  where
  gToTsvBs _ = error "Unreachable"
  {-# INLINE gToTsvBs #-}


instance {-# OVERLAPPING #-}
  ( GInsertable 'Nothing columns f
  ) => GInsertable 'Nothing columns (D1 c f)
  where
  gToTsvBs (M1 re) = gToTsvBs @'Nothing @columns re <> "\n"
  {-# INLINE gToTsvBs #-}


instance {-# OVERLAPPING #-}
  ( GInsertable 'Nothing columns f
  ) => GInsertable 'Nothing columns (C1 c f)
  where
  gToTsvBs (M1 re) = gToTsvBs @'Nothing @columns re
  {-# INLINE gToTsvBs #-}


instance {-# OVERLAPPING #-}
  ( leftCenterTreeElement ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , derivingState ~ FirstJustOrNothing
    '[ GetGenericProductHeadSelector left `AssumePlacedBefore` leftCenterTreeElement
     , leftCenterTreeElement              `AssumePlacedBefore` rightCenterTreeElement
     , rightCenterTreeElement             `AssumePlacedBefore` GetGenericProductLastSelector right
     ]
  , GInsertable derivingState firstColumnsPart left
  , GInsertable derivingState secondColumnsPart right
  ) => GInsertable 'Nothing columns (left :*: right)
  where
  gToTsvBs (left :*: right)
    =          gToTsvBs @derivingState @firstColumnsPart left
    <> "\t" <> gToTsvBs @derivingState @secondColumnsPart right
  {-# INLINE gToTsvBs #-}


instance {-# OVERLAPPING #-}
  ( Serializable chType
  , ToChType chType inputType
  , columnName ~ GetColumnName column
  , chType ~ GetColumnType column
  ) => GInsertable 'Nothing '[column]
    ( S1 (MetaSel (Just columnName) a b f) (Rec0 inputType)
    )
  where
  gToTsvBs = serialize . toChType @chType @inputType . unK1 . unM1
  {-# INLINE gToTsvBs #-}


instance
  (GInsertable
    (If (IsColumnWriteOptional anotherColumn)
      'Nothing
      ('Just
        (    'Text "Column with name " :<>: 'Text (GetColumnName anotherColumn) :<>: 'Text " is required for insert."
        :$$: 'Text "Add it to your insertable type"
        )
      )
    )
    (column ': moreColumns)
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  , ToChType (GetColumnType column) inputType
  , Serializable (GetColumnType column)
  )
  =>
  GInsertable
    'Nothing
    (column ': anotherColumn ': moreColumns)
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  where
  gToTsvBs = serialize . toChType @(GetColumnType column) @inputType . unK1 . unM1


instance
  ( GInsertable
    ('Just
      (    'Text "There is no column \"" :<>: 'Text columnName :<>: 'Text "\" in table"
      :$$: 'Text "You can't insert this field"
      )
    )
    '[]
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  ) =>
  GInsertable
    'Nothing
    '[]
    (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  where
  gToTsvBs = error "Unreachable"










-- * Selecting


class
  ( IsTable table
  ) => SelectableFrom table dataDescripion where
  default fromTsvLine
    ::
    ( GSelectable
      (TableValidationResult table)
      (GetTableColumns table)
      (Rep dataDescripion)
    , Generic dataDescripion
    ) => BS.ByteString -> dataDescripion

  fromTsvLine :: BS.ByteString -> dataDescripion
  fromTsvLine
    = to
    . gFromTsvBs
      @(TableValidationResult table)
      @(GetTableColumns table)
  {-# INLINE fromTsvLine #-}


instance {-# OVERLAPPABLE #-}
  ( IsTable table
  , Generic dataDescripion
  , TypeError
    (    'Text "You didn't provide"
    :$$: 'Text "  ( SelectableFrom"
    :$$: 'Text "    (Table \"" :<>: 'Text (GetTableName table) :<>: 'Text "\" ...)"
    :$$: 'Text "    (" :<>: ShowType dataDescripion :<>: 'Text ")"
    :$$: 'Text "  )"
    :$$: 'Text "instance"
    :$$: 'Text "Derive it via:"
    :$$: 'Text "  |data " :<>: ShowType dataDescripion
    :$$: 'Text "  |  { .."
    :$$: 'Text "  |  } deriving (Generic)"
    :$$: 'Text "  |instance SelectableFrom (Table \"" :<>: 'Text (GetTableName table)  :<>: 'Text "\" ...) " :<>: ShowType dataDescripion
    )
  ) => SelectableFrom table dataDescripion
  where
  fromTsvLine = error "Unreachable"




constructSelection :: forall table selectionDescription columnsSubset .
  ( IsSelectionDescription table selectionDescription columnsSubset
  , columnsSubset ~ GetRepresentationInColumns (GetFilters selectionDescription) (GetTableColumns table)
  ) => SelectionDescriptionConstructor table selectionDescription columnsSubset
constructSelection = consructSelectionDescription @table @selectionDescription @columnsSubset emptyDesc


data (%%) a b
infixl 4 %%

data Result a

data Variable
data EqualTo  (columnName :: Symbol) expressionValue
data HasInfix (columnName :: Symbol) expressionValue


renderSelectQuery :: SelectionDescription table description -> ByteString
renderSelectQuery (MkSelectionDescription columns table filteringParts) =
  T.encodeUtf8
    $ "SELECT " <> T.intercalate "," columns
    <> " FROM " <> table
    <> renderFilteringParts filteringParts
    <> " FORMAT TSV"
{-# INLINE renderSelectQuery #-}

data SelectionDescription table description = MkSelectionDescription
  { renderedColumns :: [Text]
  , tableName :: Text
  , _filtertingParts :: [FilteringPart]
  }

emptyDesc :: SelectionDescription table description
emptyDesc = MkSelectionDescription [] "" []

appendFilteringToSelection :: SelectionDescription table description -> Text -> SelectionDescription table description
appendFilteringToSelection (MkSelectionDescription columns table filteringParts) filteringContent
  = MkSelectionDescription columns table (MkFilteringPart filteringContent : filteringParts)

newtype FilteringPart = MkFilteringPart Text
  deriving newtype (IsString)

-- | WHERE query part rendering mechanism
--
-- >>> renderFilteringParts []
-- ""
-- >>> renderFilteringParts ["field1=\"Hello\""]
-- " WHERE field1=\"Hello\""
-- >>> renderFilteringParts ["field1=\"Hello\"", "field2=\"World\""]
-- " WHERE field2=\"World\" AND field1=\"Hello\""
renderFilteringParts :: [FilteringPart] -> Text
renderFilteringParts [MkFilteringPart part] = " WHERE " <> part
renderFilteringParts (MkFilteringPart part : otherParts) = renderFilteringParts otherParts <> " AND " <> part
renderFilteringParts [] = ""


class
  ( SelectableFrom table (ToSelectionResult description)
  ) => IsSelectionDescription table description (columnsSubset :: [Type])
  where

  type GetFilters description :: [Symbol]
  type ToSelectionResult description :: Type
  type SelectionDescriptionConstructor table description columnsSubset :: Type
  consructSelectionDescription
    :: SelectionDescription table (ToSelectionResult description)
    -> SelectionDescriptionConstructor table description columnsSubset


instance {-# OVERLAPPING #-}
  ( IsSelectionDescription table type2 columnsSubset
  , KnownSymbol columnName
  , KnownSymbol expressionValue
  ) => IsSelectionDescription table (type2 %% EqualTo columnName (expressionValue :: Symbol)) columnsSubset
  where
  type GetFilters (type2 %% EqualTo columnName (expressionValue :: Symbol)) = columnName ': GetFilters type2
  type ToSelectionResult (type2 %% EqualTo columnName expressionValue) = ToSelectionResult type2
  type SelectionDescriptionConstructor table (type2 %% EqualTo columnName expressionValue) columnsSubset = SelectionDescriptionConstructor table type2 columnsSubset
  consructSelectionDescription desc
    = consructSelectionDescription @table @type2 @columnsSubset
    . appendFilteringToSelection desc
    $ (T.pack . symbolVal $ Proxy @columnName) <> "=" <> (T.pack . symbolVal $ Proxy @expressionValue)


instance {-# OVERLAPPING #-}
  ( IsSelectionDescription table type2 columnsSubset
  , KnownSymbol columnName
  , QuerySerializable (GetColumnTypeByName columnName columnsSubset)

  ) => IsSelectionDescription table (type2 %% EqualTo columnName Variable) columnsSubset
  where
  type GetFilters (type2 %% EqualTo columnName Variable) = columnName ': GetFilters type2
  type ToSelectionResult (type2 %% EqualTo columnName Variable) = ToSelectionResult type2
  type SelectionDescriptionConstructor table (type2 %% EqualTo columnName Variable) columnsSubset = GetColumnTypeByName columnName columnsSubset -> SelectionDescriptionConstructor table type2 columnsSubset
  consructSelectionDescription desc text
    = consructSelectionDescription @table @type2 @columnsSubset
    . appendFilteringToSelection desc
    $ (T.pack . symbolVal $ Proxy @columnName) <> "=" <> T.decodeUtf8 (renderForQuery text)


instance
  ( SelectableFrom table selectableData
  ) => IsSelectionDescription table (Result selectableData) columnsSubset
  where
  type GetFilters (Result selectableData) = '[]
  type ToSelectionResult (Result selectableData) = selectableData
  type SelectionDescriptionConstructor table (Result selectableData) columnsSubset = SelectionDescription table selectableData
  consructSelectionDescription desc = desc
    { tableName = getTableName @table
    , renderedColumns = getTableRenderedColumnsNames @table
    }


type family GetColumnTypeByName
  (name :: Symbol)
  (columnsSymbol :: [Type])
  ::
  Type
  where
  GetColumnTypeByName name (column ': xs) = If (GetColumnName column == name) (GetColumnType column) (GetColumnTypeByName name xs)
  GetColumnTypeByName name '[] = TypeError
    (    'Text "Cannot find column with name \"" :<>: 'Text name :<>: 'Text "\"."
    :$$: 'Text " Report an issue if you see this message"
    )




class GSelectable
  (deivingState :: Maybe ErrorMessage)
  (columns :: [Type])
  f
  where
  gFromTsvBs :: BS.ByteString -> f p


instance
  ( TypeError errorMsg
  ) => GSelectable ('Just errorMsg) columns genericRep
  where
  gFromTsvBs _ = error "Unreachable"
  {-# INLINE gFromTsvBs #-}


instance {-# OVERLAPPING #-}
  ( GSelectable 'Nothing columns f
  ) => GSelectable 'Nothing columns (D1 c f)
  where
  gFromTsvBs bs = M1 $ gFromTsvBs @'Nothing @columns bs
  {-# INLINE gFromTsvBs #-}


instance {-# OVERLAPPING #-}
  ( GSelectable 'Nothing columns f
  ) => GSelectable 'Nothing columns (C1 c f)
  where
  gFromTsvBs = M1 . gFromTsvBs @'Nothing @columns
  {-# INLINE gFromTsvBs #-}


instance {-# OVERLAPPING #-}
  ( leftCenterTreeElement  ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , derivingState ~ FirstJustOrNothing
     '[ GetGenericProductHeadSelector left `AssumePlacedBefore` leftCenterTreeElement
      , leftCenterTreeElement              `AssumePlacedBefore` rightCenterTreeElement
      , rightCenterTreeElement             `AssumePlacedBefore` GetGenericProductLastSelector right
      ]
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , GSelectable derivingState firstColumnsPart left
  , GSelectable derivingState secondColumnsPart right
  ) => GSelectable 'Nothing columns (left :*: right)
  where
  gFromTsvBs bs = {- [ClickHaskell.DataDsl.ToDo.1]: optimize line deconstruction -}
    let byteStrings = 9 `BS.split` bs
        (leftWords, rightWords) = splitAt (length byteStrings `div` 2) byteStrings
    in  gFromTsvBs @derivingState @firstColumnsPart (BS.intercalate "\t" leftWords)
    :*: gFromTsvBs @derivingState @secondColumnsPart (BS.intercalate "\t" rightWords)
  {-# INLINE gFromTsvBs #-}


instance {-# OVERLAPPING #-}
  ( Deserializable chType
  , FromChType chType outputType
  , columnName ~ GetColumnName column
  , chType ~ GetColumnType column
  ) => GSelectable
    'Nothing
    (column ': xs)
    (S1 (MetaSel (Just columnName) a b f) (K1 i outputType))
  where
  gFromTsvBs = M1 . K1 . fromChType @chType @outputType . deserialize
  {-# INLINE gFromTsvBs #-}


instance
  ( GSelectable
    ('Just
      (    'Text "There is no column \"" :<>: 'Text columnName :<>: 'Text "\" in table."
      :$$: 'Text "You can't select this field"
      )
    )
    '[]
    (S1 (MetaSel (Just columnName) a b f) k)
  ) =>
  GSelectable
    'Nothing
    '[]
    (S1 (MetaSel (Just columnName) a b f) k)
  where
  gFromTsvBs = error "Unreachable"










-- * Generic rep validations tools


type family (sym1 :: Symbol) `AssumePlacedBefore` (sym2 :: Symbol) :: Maybe ErrorMessage
  where
  sym1 `AssumePlacedBefore` sym2 =
    If (sym1 >? sym2) 
     ('Just
      (    'Text "Record fields should be sorted alphabetically. But field \""
      :<>: 'Text sym2
      :<>: 'Text "\" placed before \""
      :<>: 'Text sym1
      :<>: 'Text "\""
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
  SpanByColumnName name columns = GoSpanByColumnName name '( columns, '[])

type family GoSpanByColumnName
  (name :: Symbol)
  (acc :: ([Type], [Type]))
  ::
  ([Type], [Type])
  where
  GoSpanByColumnName name '( '[], acc2) = '(Reverse acc2, '[])
  GoSpanByColumnName name '( column ': columns, acc2) =
    If (name <=? GetColumnName column)
      '(Reverse acc2, column ': columns)
      (GoSpanByColumnName name '(columns, column ': acc2))

type family Reverse (b :: [a]) :: [a] where Reverse list = GoReverse list '[]
type family GoReverse (list :: [a]) (acc :: [a])
  where
  GoReverse '[]       acc = acc
  GoReverse (x ': xs) acc = GoReverse xs (x ': acc)




type family GetRepresentationInColumns
  (names :: [Symbol])
  (columns :: [Type])
  ::
  [Type]
  where
  GetRepresentationInColumns '[] columns = TypeError ('Text "Report an issue if you see this message: Validation")
  GetRepresentationInColumns names columns = (GoGetRepresentationInColumns (GetMinimum names) columns)

type family GoGetRepresentationInColumns
  (minimumWithOthers :: (Symbol, [Symbol]))
  (columns :: [Type])
  ::
  [Type]
  where
  GoGetRepresentationInColumns '(min, '[])  (column ': columns) =
    If (min == GetColumnName column)
      '[column]
      (GoGetRepresentationInColumns '(min, '[]) columns)
  GoGetRepresentationInColumns '(min, xs) '[] = TypeError ('Text "Column with name " :<>: 'Text min :<>: 'Text " is not represented in table")
  GoGetRepresentationInColumns '(min, otherElems) (column ': columns) =
    If (GetColumnName column >? min)
      (TypeError ('Text "Column with name " :<>: 'Text min :<>: 'Text " is not represented2 in table"))
      (If
        (GetColumnName column == min)
        (column ': GoGetRepresentationInColumns (GetMinimum otherElems) columns)
        (GoGetRepresentationInColumns '(min, otherElems) columns)
      )

type family GetMinimum (elems :: [Symbol]) :: (Symbol, [Symbol])
  where
  GetMinimum (x ': xs) = GoGetMinimum xs '(x, '[])
  GetMinimum '[] = TypeError ('Text "No elements in list. Please report an issue")

type family GoGetMinimum
  (elems :: [Symbol])
  (acc :: (Symbol, [Symbol]))
  ::
  (Symbol, [Symbol])
  where
  GoGetMinimum (previousMin ': xs) '(previousMin, acc) = TypeError ('Text "There are duplicated filters with name \"" :<>: 'Text previousMin :<>: 'Text "\"") 
  GoGetMinimum (x ': xs) '(previousMin, acc) = If (previousMin >? x) (GoGetMinimum xs '(x, previousMin ': acc)) (GoGetMinimum xs '(previousMin, x ': acc))
  GoGetMinimum '[] '(previousMin, acc) = '(previousMin, acc)


-- * Errors handling

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
