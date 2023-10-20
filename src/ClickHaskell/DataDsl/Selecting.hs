{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
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

module ClickHaskell.DataDsl.Selecting
  ( SelectableFrom(toSelectedFrom)
  , tsvSelectQuery

  , SuchThat
  , EqualTo, HasInfix

  , ToConditionalExpression, UnwrapedDescription
  ) where

-- Internal dependencies
import ClickHaskell.DataDsl.Type (SpanByColumnName, GetGenericProductHeadSelector, GetGenericProductLastSelector, AssumePlacedBefore) 
import ClickHaskell.DbTypes      (Deserializable(deserialize), fromChType, FromChType)
import ClickHaskell.Validation   (HandleErrors)
import ClickHaskell.TableDsl     (IsLocatedTable(..), InDatabase, IsTable(..))

-- GHC included libraries imports
import Data.ByteString        as BS (ByteString)
import Data.ByteString.Char8  as BS8 (intercalate, split)
import Data.Kind              (Type)
import Data.Proxy             (Proxy(..))
import Data.Text              as T (Text, intercalate)
import Data.Text.Lazy         as T (toStrict)
import Data.Text.Lazy.Builder as T (Builder, toLazyText)
import Data.String            (IsString(fromString))
import GHC.Generics           (Generic(to, Rep), K1(K1), M1(M1), type (:*:)(..), D1, C1, S1, Meta(MetaSel))
import GHC.TypeError          (TypeError, ErrorMessage(..))
import GHC.TypeLits           (KnownSymbol, Symbol, symbolVal)


class
  ( IsTable table
  ) => SelectableFrom table dataDescripion where
  default toSelectedFrom
    ::
    ( IsTable table
    , GSelectable
      (TableValidationResult table)
      (GetTableColumns table)
      (Rep dataDescripion)
    , Generic dataDescripion
    ) => BS.ByteString -> dataDescripion
  toSelectedFrom :: BS.ByteString -> dataDescripion
  toSelectedFrom
    = to
    . gFromBs
      @(TableValidationResult table)
      @(GetTableColumns table)
  {-# INLINE toSelectedFrom #-} 


instance
  ( SelectableFrom table dataDescripion
  ) => SelectableFrom (InDatabase db table) dataDescripion
  where
  toSelectedFrom = toSelectedFrom @table


instance {-# OVERLAPPABLE #-}
  ( IsTable table
  , Generic dataDescripion
  , TypeError
    (    'Text "You didn't provide (SelectableFrom (Table \"" :<>: 'Text (GetTableName table) :<>: 'Text "\" ...) "
    :<>: ShowType dataDescripion :<>: 'Text ") instance"
    :$$: 'Text "Derive it via:"
    :$$: 'Text "  |data " :<>: ShowType dataDescripion
    :$$: 'Text "  |  { .."
    :$$: 'Text "  |  } deriving (Generic)"
    :$$: 'Text "  |instance SelectableFrom (Table \"" :<>: 'Text (GetTableName table)  :<>: 'Text "\" ...) " :<>: ShowType dataDescripion
    )
  ) => SelectableFrom table dataDescripion
  where
  toSelectedFrom = error "Unreachable"



type family UnwrapedDescription t :: Type where
  UnwrapedDescription (SuchThat fieldName conditionalExpression handlingData) = UnwrapedDescription handlingData
  UnwrapedDescription handlingData = handlingData




class ToConditionalExpression t where
  toConditionalExpression :: Builder

instance
  ( ToConditionalExpression (SuchThat fieldName2 conditionalExpPart2 handlingData)
  , ToConditionalExpPart conditionalExpPart
  , ToConditionalExpPart conditionalExpPart2
  , KnownSymbol fieldName
  , KnownSymbol fieldName2
  ) => ToConditionalExpression
    (fieldName `SuchThat` conditionalExpPart
      (SuchThat fieldName2 conditionalExpPart2 handlingData)
    )
  where
  toConditionalExpression
    =  fromString (symbolVal (Proxy @fieldName)) <> "=" <> toConditionalExpPart @conditionalExpPart
    <> " AND "
    <> toConditionalExpression @((fieldName2 `SuchThat` conditionalExpPart2) handlingData)

instance
  ( ToConditionalExpPart conditionalExpPart
  , KnownSymbol fieldName
  ) => ToConditionalExpression (SuchThat fieldName conditionalExpPart handlingData)
  where
  toConditionalExpression = fromString (symbolVal (Proxy @fieldName)) <> toConditionalExpPart @conditionalExpPart

instance {-# OVERLAPPABLE #-}
  ToConditionalExpression handlingData
  where
  toConditionalExpression = ""




class ToConditionalExpPart a where
  toConditionalExpPart :: T.Builder 


data SuchThat (fieldName :: Symbol) (conditionalExpression :: Type) handlingData


data EqualTo (a :: Symbol)
instance
  ( KnownSymbol a
  ) => ToConditionalExpPart (EqualTo a)
  where
  toConditionalExpPart = "='" <> fromString (symbolVal (Proxy @a)) <> "'"

data HasInfix (a :: Symbol)
instance
  ( KnownSymbol a
  ) => ToConditionalExpPart (HasInfix a)
  where
  toConditionalExpPart = " like '" <> fromString (symbolVal (Proxy @a)) <> "%'"




tsvSelectQuery :: forall
  handlingDataDescripion locatedTable .
  ( IsLocatedTable locatedTable
  , SelectableFrom locatedTable (UnwrapedDescription handlingDataDescripion)
  , ToConditionalExpression handlingDataDescripion
  ) => Text
tsvSelectQuery
  =  "SELECT " <> columnsMapping
  <> " FROM " <> getDatabaseName @locatedTable <> "." <> getTableName @locatedTable
  <> " " <> (if whereConditions=="" then "" else "WHERE " <> whereConditions)
  <> " FORMAT TSV"
  where
  whereConditions
    = T.toStrict
    . T.toLazyText 
    $ toConditionalExpression @handlingDataDescripion
  columnsMapping
    = T.intercalate ","
    . map fst
    $ getTableRenderedColumns @locatedTable
{-# INLINE tsvSelectQuery #-}




class GSelectable
  (deivingState :: (Bool, ErrorMessage))
  (columns :: [(Symbol, Type)])
  f
  where
  gFromBs :: BS.ByteString -> f p


instance
  ( TypeError errorMsg
  ) => GSelectable '(True, errorMsg) columns genericRep where
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
  ( firstTreeElement ~ GetGenericProductHeadSelector left
  , leftCenterTreeElement ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , lastTreeElement ~ GetGenericProductLastSelector right
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , derivingState ~ HandleErrors
    '[ firstTreeElement       `AssumePlacedBefore` leftCenterTreeElement
     , leftCenterTreeElement  `AssumePlacedBefore` rightCenterTreeElement
     , rightCenterTreeElement `AssumePlacedBefore` lastTreeElement
     ]
  , GSelectable derivingState firstColumnsPart left
  , GSelectable derivingState secondColumnsPart right
  ) => GSelectable '(False, unreachableError) columns (left :*: right)
  where
  gFromBs bs = {- [ClickHaskell.DataDsl.Selecting.ToDo.1]: optimize line deconstruction -}
    let byteStrings = '\t' `BS8.split` bs
        (leftWords, rightWords) = splitAt (length byteStrings `div` 2) byteStrings
    in  gFromBs @derivingState @firstColumnsPart (BS8.intercalate "\t" leftWords)
    :*: gFromBs @derivingState @secondColumnsPart (BS8.intercalate "\t" rightWords)
  {-# INLINE gFromBs #-}


instance {-# OVERLAPPING #-}
  ( Deserializable chType
  , FromChType chType outputType
  ) => GSelectable '(False, unrechableError) '[ '(columnName, chType)]
    ( S1 (MetaSel (Just columnName) a b f) (K1 i outputType)
    )
  where
  gFromBs = M1 . K1 . fromChType @chType @outputType . deserialize
  {-# INLINE gFromBs #-}


instance
  ( TypeError ('Text "Not found column with name \"" :<>: 'Text columnName :<>: 'Text "\" in table")
  ) => GSelectable '(False, unrechableError) '[]
    ( S1 (MetaSel (Just columnName) a b f) (K1 i outputType)
    )
  where
  gFromBs = error "Unreachable"
