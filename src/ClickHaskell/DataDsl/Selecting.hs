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
  , httpStreamChSelect
  , tsvSelectQuery

  , SuchThat
  , EqualTo, HasInfix
  ) where

-- Internal dependencies
import ClickHaskell.Client       (HttpChClient(..), ChException (..))
import ClickHaskell.DataDsl.Type (SpanByColumnName, GetGenericProductHeadSelector, GetGenericProductLastSelector, HandleSeveralErrors, AssumePlacedAfter) 
import ClickHaskell.DbTypes      (Deserializable(deserialize), fromChType, FromChType)
import ClickHaskell.TableDsl     (ShowColumns, KnownTupleSymbols(..), IsLocatedTable(..), InDatabase, IsTable(..))

-- External dependenices
import Conduit              (yield)
import Network.HTTP.Client  as H (Request(..), Response(..), httpLbs)
import Network.HTTP.Conduit as H (requestBodySourceChunked)
import Network.HTTP.Types   as H (Status(..))

-- GHC included libraries imports
import Control.Exception          (throw)
import Control.Monad              (when)
import Data.ByteString            as BS (ByteString, toStrict)
import Data.ByteString.Char8      as BS8 (intercalate, split)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.Kind                  (Type)
import Data.Proxy                 (Proxy(..))
import Data.Text                  as T (Text, intercalate)
import Data.Text.Encoding         as T (encodeUtf8, decodeUtf8)
import Data.Text.Lazy             as T (toStrict)
import Data.Text.Lazy.Builder     as T (Builder, toLazyText)
import Data.String                (IsString(fromString))
import GHC.Generics               (Generic(to, Rep), K1(K1), M1(M1), type (:*:)(..), D1, C1, S1, Rec0, Meta(MetaSel))
import GHC.TypeError              (TypeError, ErrorMessage(..))
import GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)


class
  ( IsTable table
  ) => SelectableFrom table dataDescripion where
  default toSelectedFrom
    ::
    ( IsTable table
    , GSelectable
      '(False, 'Text "Report an issue if you see this message ClickHaskell.DataDsl.Selecting.1")
      "entrypoint"
      (GetTableColumns table)
      (Rep dataDescripion)
    , Generic dataDescripion
    ) => BS.ByteString -> dataDescripion
  toSelectedFrom :: BS.ByteString -> dataDescripion
  toSelectedFrom
    = to
    . gFromBs
      @'(False, 'Text "Report an issue if you see this message ClickHaskell.DataDsl.Selecting.1")
      @"entrypoint"
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




httpStreamChSelect :: forall handlingDataDescripion locatedTable .
  ( IsLocatedTable locatedTable
  , SelectableFrom locatedTable (UnwrapedDescription handlingDataDescripion)
  , ToConditionalExpression handlingDataDescripion
  , KnownTupleSymbols (ShowColumns (GetColumns (Rep (UnwrapedDescription handlingDataDescripion))))
  ) => HttpChClient -> IO [UnwrapedDescription handlingDataDescripion]
httpStreamChSelect (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { H.requestBody = H.requestBodySourceChunked
      $ yield (encodeUtf8 $ tsvSelectQuery @handlingDataDescripion @locatedTable)
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure
    . map (toSelectedFrom @locatedTable . BS.toStrict)
    . BSL8.lines
    $ responseBody resp




tsvSelectQuery :: forall
  handlingDataDescripion locatedTable .
  ( IsLocatedTable locatedTable
  , SelectableFrom locatedTable (UnwrapedDescription handlingDataDescripion)
  , ToConditionalExpression handlingDataDescripion
  , KnownTupleSymbols (ShowColumns (GetColumns (Rep (UnwrapedDescription handlingDataDescripion))))
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
    $ symbolsTupleVals @(ShowColumns (GetColumns (Rep (UnwrapedDescription handlingDataDescripion))))
{-# INLINE tsvSelectQuery #-}




type family GetColumns (t :: k -> Type) :: [(Symbol, Type)] where
  GetColumns (D1 _ cons) = GetColumns cons
  GetColumns (C1 _ sels) = GetColumns sels
  GetColumns (c :*: c2) = GetColumns c ++ GetColumns c2
  GetColumns (S1 (MetaSel (Just sel) _ _ f) (Rec0 t)) = '[ '(sel, t)]

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)



class GSelectable (deivingState :: (Bool, ErrorMessage)) (cmpElement :: Symbol) (columns :: [(Symbol, Type)]) f where
  gFromBs :: BS.ByteString -> f p


instance
  ( GSelectable derivingState "entrypoint" columns f
  ) => GSelectable derivingState "entrypoint" columns (D1 c f) where
  gFromBs bs = M1 $ gFromBs @derivingState @"entrypoint" @columns bs
  {-# INLINE gFromBs #-}


instance
  ( firstTreeElement ~ GetGenericProductHeadSelector left
  , leftCenterTreeElement ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , GSelectable derivingState firstTreeElement firstColumnsPart left
  , GSelectable derivingState rightCenterTreeElement secondColumnsPart right
  , derivingState ~ HandleSeveralErrors
    '[ firstTreeElement `AssumePlacedAfter` rightCenterTreeElement
     ]
  ) => GSelectable '(False, unreachableError) "entrypoint" columns (C1 c (left :*: right)) where
  gFromBs bs = {- [ClickHaskell.DataDsl.Selecting.ToDo.1]: optimize string deconstruction -}
    let byteStrings = '\t' `BS8.split` bs
        (firstWords, lastWords) = splitAt (length byteStrings `div` 2) byteStrings
    in
    M1
      (   gFromBs @derivingState @firstTreeElement @firstColumnsPart (BS8.intercalate "\t" firstWords)
      :*: gFromBs @derivingState @rightCenterTreeElement @secondColumnsPart (BS8.intercalate "\t" lastWords)
      )
  {-# INLINE gFromBs #-}

instance
  ( TypeError errorMsg
  ) => GSelectable '(True, errorMsg) "entrypoint" columns (C1 c (left :*: right)) where
  gFromBs _ = error "Unreachable"
  {-# INLINE gFromBs #-}

instance
  ( TypeError errorMsg
  ) => GSelectable '(True, errorMsg) cmpElement columns (left :*: right)
  where
  gFromBs _ = error "Unreachable"


instance
  ( firstTreeElement ~ GetGenericProductHeadSelector left
  , leftCenterTreeElement ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , GSelectable derivingState leftCenterTreeElement firstColumnsPart left
  , GSelectable derivingState rightCenterTreeElement secondColumnsPart right
  , derivingState ~ HandleSeveralErrors
    '[ firstTreeElement `AssumePlacedAfter` rightCenterTreeElement
     , firstTreeElement `AssumePlacedAfter` rightCenterTreeElement
     ]
  ) => GSelectable '(False, unreachableError) cmpElement columns (left :*: right)
  where
  gFromBs bs = {- [ClickHaskell.DataDsl.Selecting.ToDo.1]: optimize string deconstruction -}
    let byteStrings = '\t' `BS8.split` bs
        (leftWords, rightWords) = splitAt (length byteStrings `div` 2) byteStrings
    in  gFromBs @derivingState @leftCenterTreeElement @firstColumnsPart (BS8.intercalate "\t" leftWords)
    :*: gFromBs @derivingState @rightCenterTreeElement @secondColumnsPart (BS8.intercalate "\t" rightWords)
  {-# INLINE gFromBs #-}



instance
  ( TypeError errorMsg
  ) => GSelectable '(True, errorMsg) cmpElement anyList
    ( S1 (MetaSel (Just columnName) a b f) (K1 i outputType)
    )
  where
  gFromBs _ = error "Unreachable"

instance
  ( Deserializable chType
  , FromChType chType outputType
  ) => GSelectable '(False, unrechableError) cmpElement '[ '(columnName, chType)]
    ( S1 (MetaSel (Just columnName) a b f) (K1 i outputType)
    )
  where
  gFromBs = M1 . K1 . fromChType @chType @outputType . deserialize
  {-# INLINE gFromBs #-}


instance
  ( TypeError ('Text "Not found column with name " :<>: 'Text sel)
  ) => GSelectable '(False, unrechableError) cmpElement '[]
    ( S1 (MetaSel (Just sel) a b f) (K1 i outputType)
    )
  where
  gFromBs = error "Unreachable"
  {-# INLINE gFromBs #-}
