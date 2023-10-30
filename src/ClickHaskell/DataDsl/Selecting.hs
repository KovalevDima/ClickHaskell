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

module ClickHaskell.DataDsl.Selecting
  ( SelectableFrom(deserializeSelectionResult)
  , renderSelectQuery

  , SelectionDescription
  , IsSelectionDescription(SelectionDescriptionConstructor, ToSelectionResult, GetFilters)
  , constructSelection

  , type (%%)
  , Result
  , EqualTo, HasInfix
  , Variable
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes    (Deserializable(deserialize), fromChType, FromChType, QuerySerializable (renderForQuery))
import ClickHaskell.Validation (HandleErrors, SpanByColumnName, GetGenericProductHeadSelector, GetGenericProductLastSelector, AssumePlacedBefore, GetRepresentationInColumns)
import ClickHaskell.TableDsl   (IsLocatedTable(..), IsTable(..), InDatabase)

-- GHC included libraries imports
import Data.ByteString    as BS (ByteString, intercalate, split)
import Data.Data          (Proxy(..))
import Data.Kind          (Type)
import Data.Text          as T (Text, intercalate, pack)
import Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import Data.String        (IsString(..))
import GHC.Generics       (Generic(to, Rep), K1(K1), M1(M1), type (:*:)(..), D1, C1, S1, Meta(MetaSel))
import GHC.TypeError      (TypeError, ErrorMessage(..))
import GHC.TypeLits       (Symbol, KnownSymbol, symbolVal)




class
  ( IsTable table
  ) => SelectableFrom table dataDescripion where
  default deserializeSelectionResult
    ::
    ( GSelectable
      (TableValidationResult table)
      (GetTableColumns table)
      (Rep dataDescripion)
    , Generic dataDescripion
    ) => BS.ByteString -> dataDescripion

  deserializeSelectionResult :: BS.ByteString -> dataDescripion
  deserializeSelectionResult
    = to
    . gFromBs
      @(TableValidationResult table)
      @(GetTableColumns table)
  {-# INLINE deserializeSelectionResult #-}


instance
  ( IsTable table
  , SelectableFrom table dataDescripion
  ) => SelectableFrom (InDatabase dbname table) dataDescripion
  where
  deserializeSelectionResult = deserializeSelectionResult @table @dataDescripion


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
  deserializeSelectionResult = error "Unreachable"






constructSelection :: forall table selectionDescription columnsSubset .
  ( IsSelectionDescription table selectionDescription columnsSubset
  , columnsSubset ~ GetRepresentationInColumns (GetFilters selectionDescription) (GetTableColumns table)
  ) => SelectionDescriptionConstructor table selectionDescription columnsSubset
constructSelection = consructSelectionDescription @table @selectionDescription @columnsSubset emptyDesc


class
  ( IsLocatedTable table
  , SelectableFrom table (ToSelectionResult selectionDescription)
  ) => IsSelectionDescription table selectionDescription (columnsSubset :: [(Symbol, Type)])
  where

  type GetFilters selectionDescription :: [Symbol]
  type ToSelectionResult selectionDescription :: Type
  type SelectionDescriptionConstructor table selectionDescription columnsSubset :: Type
  consructSelectionDescription
    :: SelectionDescription table (ToSelectionResult selectionDescription)
    -> SelectionDescriptionConstructor table selectionDescription columnsSubset


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


instance {-# OVERLAPPING #-}
  ( IsSelectionDescription table type2 columnsSubset
  , KnownSymbol columnName
  , KnownSymbol expressionValue
  ) => IsSelectionDescription table (type2 %% HasInfix columnName (expressionValue :: Symbol)) columnsSubset
  where
  type GetFilters (type2 %% HasInfix columnName expressionValue) = columnName ': GetFilters type2
  type ToSelectionResult (type2 %% HasInfix columnName expressionValue) = ToSelectionResult type2
  type SelectionDescriptionConstructor table (type2 %% HasInfix columnName expressionValue) columnsSubset = SelectionDescriptionConstructor table type2 columnsSubset
  consructSelectionDescription desc
    = consructSelectionDescription @table @type2 @columnsSubset
    . appendFilteringToSelection desc
    $ (T.pack . symbolVal $ Proxy @columnName) <> "=" <> (T.pack . symbolVal $ Proxy @expressionValue)


instance {-# OVERLAPPING #-}
  ( IsSelectionDescription table type2 columnsSubset
  , KnownSymbol columnName
  , QuerySerializable (GetColumnTypeByName columnName columnsSubset)
  ) => IsSelectionDescription table (type2 %% HasInfix columnName Variable) columnsSubset
  where
  type GetFilters (type2 %% HasInfix columnName Variable) = columnName ': GetFilters type2
  type ToSelectionResult (type2 %% HasInfix columnName Variable) = ToSelectionResult type2
  type SelectionDescriptionConstructor table (type2 %% HasInfix columnName Variable) columnsSubset = GetColumnTypeByName columnName columnsSubset -> SelectionDescriptionConstructor table type2 columnsSubset
  consructSelectionDescription desc text
    = consructSelectionDescription @table @type2 @columnsSubset
    . appendFilteringToSelection desc
    $ (T.pack . symbolVal $ Proxy @columnName) <> "=" <> T.decodeUtf8 (renderForQuery text)


instance
  ( SelectableFrom table selectableData
  , IsLocatedTable table
  ) => IsSelectionDescription table (Result selectableData) columnsSubset
  where
  type GetFilters (Result selectableData) = '[]
  type ToSelectionResult (Result selectableData) = selectableData
  type SelectionDescriptionConstructor table (Result selectableData) columnsSubset = SelectionDescription table selectableData
  consructSelectionDescription desc = desc
    { tableName = getTableName @table
    , dbName = getDatabaseName @table
    , renderedColumns = getTableRenderedColumnsNames @table
    }


type family GetColumnTypeByName
  (name :: Symbol)
  (columnsSymbol :: [(Symbol, Type)])
  ::
  Type
  where
  GetColumnTypeByName name ('(name, columnType) ': xs) = columnType
  GetColumnTypeByName name ('(notTheSameName, _) ': xs) = GetColumnTypeByName name xs
  GetColumnTypeByName name '[] = TypeError
    (    'Text "Cannot find column with name \"" :<>: 'Text name :<>: 'Text "\"."  
    :$$: 'Text " Report an issue if you see this message"
    )






data (%%) a b
infixl 4 %%

data Result a

data Variable
data EqualTo  (columnName :: Symbol) expressionValue
data HasInfix (columnName :: Symbol) expressionValue






renderSelectQuery :: SelectionDescription table description -> ByteString
renderSelectQuery (MkSelectionDescription columns db table filteringParts) =
  T.encodeUtf8
    $ "SELECT " <> T.intercalate "," columns
    <> " FROM " <> db <> "." <> table
    <> renderFilteringParts filteringParts
    <> " FORMAT TSV"
{-# INLINE renderSelectQuery #-}

data SelectionDescription table description = MkSelectionDescription
  { renderedColumns :: [Text]
  , dbName :: Text
  , tableName :: Text
  , _filtertingParts :: [FilteringPart]
  }

emptyDesc :: SelectionDescription table description
emptyDesc = MkSelectionDescription [] "" "" []

appendFilteringToSelection :: SelectionDescription table description -> Text -> SelectionDescription table description
appendFilteringToSelection (MkSelectionDescription columns db table filteringParts) filteringContent
  = MkSelectionDescription columns db table (MkFilteringPart filteringContent : filteringParts)

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






class GSelectable
  (deivingState :: (Bool, ErrorMessage))
  (columns :: [(Symbol, Type)])
  f
  where
  gFromBs :: BS.ByteString -> f p


instance
  ( TypeError errorMsg
  ) => GSelectable '(True, errorMsg) columns genericRep
  where
  gFromBs _ = error "Unreachable"
  {-# INLINE gFromBs #-}


instance {-# OVERLAPPING #-}
  ( GSelectable '(False, unreachableError) columns f
  ) => GSelectable '(False, unreachableError) columns (D1 c f)
  where
  gFromBs bs = M1 $ gFromBs @'(False, unreachableError) @columns bs
  {-# INLINE gFromBs #-}


instance {-# OVERLAPPING #-}
  ( GSelectable '(False, unreachableError) columns (left :*: right)
  ) => GSelectable '(False, unreachableError) columns (C1 c (left :*: right))
  where
  gFromBs = M1 . gFromBs @'(False, unreachableError) @columns
  {-# INLINE gFromBs #-}


instance {-# OVERLAPPING #-}
  ( firstTreeElement       ~ GetGenericProductHeadSelector left
  , leftCenterTreeElement  ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , lastTreeElement        ~ GetGenericProductLastSelector right
  , derivingState ~ HandleErrors
     '[ firstTreeElement `AssumePlacedBefore` leftCenterTreeElement
      , leftCenterTreeElement `AssumePlacedBefore` rightCenterTreeElement
      , rightCenterTreeElement `AssumePlacedBefore` lastTreeElement
      ]
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , GSelectable derivingState firstColumnsPart left
  , GSelectable derivingState secondColumnsPart right
  ) => GSelectable '(False, unreachableError) columns (left :*: right)
  where
  gFromBs bs = {- [ClickHaskell.DataDsl.Selecting.ToDo.1]: optimize line deconstruction -}
    let byteStrings = 9 `BS.split` bs
        (leftWords, rightWords) = splitAt (length byteStrings `div` 2) byteStrings
    in  gFromBs @derivingState @firstColumnsPart (BS.intercalate "\t" leftWords)
    :*: gFromBs @derivingState @secondColumnsPart (BS.intercalate "\t" rightWords)
  {-# INLINE gFromBs #-}


instance {-# OVERLAPPING #-}
  ( Deserializable chType
  , FromChType chType outputType
  ) => GSelectable
    '(False, unrechableError)
    '[ '(columnName, chType)]
    (S1 (MetaSel (Just columnName) a b f) (K1 i outputType))
  where
  gFromBs = M1 . K1 . fromChType @chType @outputType . deserialize
  {-# INLINE gFromBs #-}


instance
  ( TypeError ('Text "Not found column with name \"" :<>: 'Text columnName :<>: 'Text "\" in table")
  ) => GSelectable
    '(False, unrechableError)
    '[]
    (S1 (MetaSel (Just columnName) a b f) (K1 i outputType))
  where
  gFromBs = error "Unreachable"
