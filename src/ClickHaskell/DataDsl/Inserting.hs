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

module ClickHaskell.DataDsl.Inserting
  ( InsertableInto(toInsertableInto)
  , httpStreamChInsert
  
  , tsvInsertQueryHeader
  ) where

-- Internal dependencies
import ClickHaskell.Client       (ChException(ChException), HttpChClient(..))
import ClickHaskell.DataDsl.Type (GetGenericProductHeadSelector, SpanByColumnName)
import ClickHaskell.DbTypes      (Serializable(serialize), ToChType(toChType))
import ClickHaskell.TableDsl     (InDatabase, KnownTupleSymbols(symbolsTupleVals), ShowColumns, IsLocatedTable(getDatabaseName), IsTable(..))

-- External dependenices
import Conduit              (yield, yieldMany)
import Network.HTTP.Types   as H (Status(..))
import Network.HTTP.Conduit as H (Request(..), Response(..), httpLbs, requestBodySourceChunked)

-- GHC included libraries imports
import Control.Exception    (throw)
import Control.Monad        (when)
import Data.ByteString      as BS (ByteString, toStrict)
import Data.ByteString.Lazy as BSL (ByteString)
import Data.Kind            (Type)
import Data.Text            as T (Text, intercalate)
import Data.Text.Encoding   as T (decodeUtf8, encodeUtf8)
import GHC.Generics         (Generic, (:*:)(..), Meta(MetaSel), S1, C1, D1, Generic(..), M1 (..), K1, unK1, Rec0)
import GHC.TypeLits         (ErrorMessage (..), TypeError, Symbol)


class
  ( IsTable table
  ) => InsertableInto table insertableData where
  default toInsertableInto
    ::
    ( IsTable table
    , Generic insertableData
    , GInsertable (GetTableColumns table) (Rep insertableData)
    ) => insertableData -> BS.ByteString
  toInsertableInto :: insertableData -> BS.ByteString
  toInsertableInto = gToBs @(GetTableColumns table) . from
  {-# INLINE toInsertableInto #-}


instance {-# OVERLAPPING #-}
  ( InsertableInto table insertableData
  ) => InsertableInto (InDatabase dbName table) insertableData
  where
  toInsertableInto = toInsertableInto @table


instance  {-# OVERLAPPABLE #-}
  ( IsTable table
  , Generic insertableData
  , GInsertable (GetTableColumns table) (Rep insertableData)
  , TypeError
    (    'Text "You didn't provide (InsertableInto (Table \"" :<>: 'Text (GetTableName table) :<>: 'Text "\" ...) "
    :<>: ShowType insertableData :<>: 'Text ") instance"
    :$$: 'Text "Derive it via:"
    :$$: 'Text "  |data " :<>: ShowType insertableData
    :$$: 'Text "  |  { .."
    :$$: 'Text "  |  } deriving (Generic)"
    :$$: 'Text "  |instance InsertableInto (Table \"" :<>: 'Text (GetTableName table)  :<>: 'Text "\" ...) " :<>: ShowType insertableData
    )
  ) => InsertableInto table insertableData




httpStreamChInsert :: forall locatedTable handlingDataDescripion .
  ( KnownTupleSymbols (ShowColumns (GetTableColumns locatedTable))
  , InsertableInto locatedTable handlingDataDescripion
  , IsLocatedTable locatedTable
  , IsTable locatedTable
  ) => HttpChClient -> [handlingDataDescripion] -> IO (H.Response BSL.ByteString)
httpStreamChInsert (HttpChClient man req) schemaList = do
  resp <- H.httpLbs
    req
      { requestBody = H.requestBodySourceChunked $
        yield     (encodeUtf8 (tsvInsertQueryHeader @locatedTable @handlingDataDescripion))
        >> yieldMany (map (toInsertableInto @locatedTable) schemaList)
      }
    man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure resp




tsvInsertQueryHeader :: forall locatedTable handlingDataDescripion .
  ( KnownTupleSymbols (ShowColumns (GetTableColumns locatedTable))
  , InsertableInto locatedTable handlingDataDescripion
  , IsLocatedTable locatedTable
  , IsTable locatedTable
  ) => Text
tsvInsertQueryHeader =
  let columnsMapping = T.intercalate "," . map fst $ symbolsTupleVals @(ShowColumns (GetTableColumns locatedTable))
  in "INSERT INTO " <> getDatabaseName @locatedTable <> "." <> getTableName @locatedTable
  <> " (" <> columnsMapping <> ")"
  <> " FORMAT TSV\n"




class GInsertable (columns :: [(Symbol, Type)]) f where
  gToBs :: f p -> BS.ByteString


instance
  ( GInsertable columns f
  ) => GInsertable columns (D1 c f)
  where
  gToBs (M1 re) = gToBs @columns re <> "\n"
  {-# INLINE gToBs #-}


instance
  ( GInsertable columns f
  ) => GInsertable columns (C1 c f)
  where
  gToBs (M1 re) = gToBs @columns re
  {-# INLINE gToBs #-}


instance
  ( GInsertable firstColumnsPart f
  , GInsertable secondColumnsPart f2
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName (GetGenericProductHeadSelector f2) columns
  ) => GInsertable columns (f :*: f2)
  where
  gToBs (f :*: f2)
    =          gToBs @firstColumnsPart f
    <> "\t" <> gToBs @secondColumnsPart f2
  {-# INLINE gToBs #-}


instance
  ( Serializable chType
  , ToChType chType inputType
  ) => GInsertable '[ '(columnName, chType)] (S1 (MetaSel (Just columnName) a b f) (Rec0 inputType))
  where
  gToBs (M1 re) = serialize . toChType @chType @inputType $ unK1 re
  {-# INLINE gToBs #-}

instance
  ( TypeError
    (    'Text "Columns: "
    :$$: ShowType (someElem ': moreColumns)
    :$$: 'Text "required for insert."
    :$$: 'Text "Add them to your insertable type")
  ) => GInsertable  ('(otherColumnName, chType) ': someElem ': moreColumns) (S1 columnName (K1 i inputType))
  where
  gToBs _ = error "Unreachable"

instance
  ( TypeError ('Text "There is no column " :<>: 'Text columnName :<>: 'Text " found in table")
  ) => GInsertable  '[] (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  where
  gToBs = error "Unreachable"
