{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , UndecidableInstances
  #-}
module ClickHaskell.DataDsl
  ( HasChSchema

  , SampledBy
  , EqualityWith, Infixion

  , ValidatedRequestedColumns

  , httpStreamChSelect, tsvSelectQuery
  , httpStreamChInsert
  ) where


-- Internal dependencies
import ClickHaskell.Client           (ChException(ChException), HttpChClient(..))
import ClickHaskell.TableDsl         (InDatabase, IsTable, SupportedAndVerifiedColumns, ShowColumns, symbolsTupleVals, KnownTupleSymbols, Table)
import ClickHaskell.TableDsl.DbTypes (IsChType(..))

-- External dependenices
import Conduit              (yield, yieldMany)
import Network.HTTP.Types   as H (Status(..))
import Network.HTTP.Conduit as H (Request(..), Response(..), httpLbs, requestBodySourceChunked)

-- GHC included libraries imports
import Control.Exception          (throw)
import Control.Monad              (when)
import Data.ByteString            as BS (ByteString, toStrict)
import Data.ByteString.Char8      as BS8 (split, intercalate)
import Data.ByteString.Lazy       as BSL (ByteString, toStrict)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.Data                  (Proxy (..))
import Data.Kind                  (Type)
import Data.Text                  as T (Text, intercalate, pack)
import Data.Text.Encoding         as T (decodeUtf8, encodeUtf8)
import Data.Text.Lazy             as T (toStrict)
import Data.Text.Lazy.Builder     as T (toLazyText, Builder, fromString)
import GHC.Generics               (Generic, (:*:)(..), S1, C1, D1, M1(..), K1(..), Generic(..), Meta(..), Rec0)
import GHC.TypeLits               (KnownSymbol, symbolVal, Symbol, ErrorMessage (..), TypeError)




type ValidatedRequestedColumns :: Type -> columns -> [(Symbol, Symbol)]
type family ValidatedRequestedColumns handlingDataDescripion table where
  ValidatedRequestedColumns handlingDataDescripion columns
    = ShowColumns
      ( ValidatedSubset
        (GetColumns (Rep handlingDataDescripion))
        (SupportedAndVerifiedColumns columns)
      )


type ValidatedSubset :: [(Symbol, Type)] -> [(Symbol, Type)] -> [(Symbol, Type)]
type family ValidatedSubset a b where
  ValidatedSubset ('(field, fieldType) ': cols1) cols2 = ColumnSolve '(field, fieldType) cols2 ': ValidatedSubset cols1 cols2
  ValidatedSubset '[] cols2 = '[]


type ColumnSolve :: (Symbol, Type) -> [(Symbol, Type)] -> (Symbol, Type)
type family ColumnSolve a as where
  ColumnSolve '(field, chType) ('(field, chType) ': fields)  =  '(field, chType)
  ColumnSolve '(field, chType) ('(field, chType2) ': fields) = TypeError ('Text "There is a field " ':<>: 'Text field ':<>: 'Text " had different type declarations")
  ColumnSolve '(field, _) '[]                                = TypeError ('Text "There is a field " ':<>: 'Text field ':<>: 'Text " that is out of table")
  ColumnSolve column (column2 ': columns)                    = ColumnSolve column columns




type GetColumns :: (k -> Type) -> [(Symbol, Type)]
type family GetColumns t where
  GetColumns (D1 _ cons) = GetColumns cons
  GetColumns (C1 _ sels) = GetColumns sels
  GetColumns (c :*: c2) = GetColumns c ++ GetColumns c2
  GetColumns (S1 (MetaSel (Just sel) _ _ f) (Rec0 t)) = '[ '(sel, t)]

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)




instance (HasChSchema handlingData)
  => HasChSchema (SampledBy fieldName conditionalExpression handlingData) where
  toBs :: HasChSchema handlingData => SampledBy fieldName conditionalExpression handlingData -> BS.ByteString
  toBs (MkSampledBy handlingData) = toBs handlingData

  fromBs :: HasChSchema handlingData => BS.ByteString -> SampledBy fieldName conditionalExpression handlingData
  fromBs bs = MkSampledBy $ fromBs bs


type Unwraped :: Type -> Type
type family Unwraped t where
  Unwraped (SampledBy fieldName conditionalExpression handlingData) = Unwraped handlingData
  Unwraped handlingData = handlingData




class ToConditionalExpression t where
  toConditionalExpression :: Builder

instance
  ( ToConditionalExpression (SampledBy fieldName2 conditionalExpPart2 handlingData)
  , ToConditionalExpPart conditionalExpPart
  , ToConditionalExpPart conditionalExpPart2
  , KnownSymbol fieldName
  , KnownSymbol fieldName2
  ) => 
  ToConditionalExpression (SampledBy fieldName conditionalExpPart)
  where
  toConditionalExpression
    =  fromString (symbolVal (Proxy @fieldName))  <> "=" <> toConditionalExpPart @conditionalExpPart
    <> " AND "
    <> toConditionalExpression @(SampledBy fieldName2 conditionalExpPart2 handlingData)

instance
  ( HasChSchema handlingData
  , ToConditionalExpPart conditionalExpPart
  , KnownSymbol fieldName
  ) =>
  ToConditionalExpression (SampledBy fieldName conditionalExpPart handlingData)
  where
  toConditionalExpression = fromString (symbolVal (Proxy @fieldName)) <> "=" <> toConditionalExpPart @conditionalExpPart

instance {-# OVERLAPPABLE #-}
  ( HasChSchema handlingData
  ) =>
  ToConditionalExpression handlingData
  where
  toConditionalExpression = ""




class ToConditionalExpPart a where
  toConditionalExpPart :: Builder 


data SampledBy (fieldName :: Symbol) (conditionalExpression :: Type) handlingData where 
  MkSampledBy :: handlingData -> SampledBy fieldName conditionalExpression handlingData
  deriving (Generic, Show, Functor)


data EqualityWith (a :: Symbol)
instance KnownSymbol a
  => ToConditionalExpPart (EqualityWith a) where
  toConditionalExpPart = "='" <> fromString (symbolVal (Proxy @a)) <> "'"

data Infixion      a



httpStreamChSelect :: forall handlingDataDescripion locatedTable db table name columns engine partitionBy orderBy .
  ( HasChSchema (Unwraped handlingDataDescripion)
  , KnownTupleSymbols (ValidatedRequestedColumns (Unwraped handlingDataDescripion) columns)
  , IsTable table name columns engine partitionBy orderBy
  , locatedTable ~ InDatabase db table
  , KnownSymbol db
  , ToConditionalExpression handlingDataDescripion
  )
  => HttpChClient -> IO [Unwraped handlingDataDescripion]
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
    . map (fromBs . BSL.toStrict)
    . BSL8.lines
    $ responseBody resp

tsvSelectQuery :: forall
  handlingDataDescripion tableWithDb
  db
  table name columns engine partitionBy orderBy
  .
  ( HasChSchema (Unwraped handlingDataDescripion)
  , KnownTupleSymbols (ValidatedRequestedColumns (Unwraped handlingDataDescripion) columns)
  , ToConditionalExpression handlingDataDescripion
  , IsTable table name columns engine partitionBy orderBy
  , tableWithDb ~ InDatabase db table
  , KnownSymbol db
  ) => Text
tsvSelectQuery =
  let columnsMapping = T.intercalate "," . map fst $ symbolsTupleVals @(ValidatedRequestedColumns (Unwraped handlingDataDescripion) columns)
      whereConditions = T.toStrict (T.toLazyText $ toConditionalExpression @handlingDataDescripion)
  in  "SELECT " <> columnsMapping
  <> " FROM " <> (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name)
  <> " " <> (if whereConditions=="" then "" else "WHERE " <> whereConditions)
  <> " FORMAT TSV"
{-# INLINE tsvSelectQuery #-}


httpStreamChInsert :: forall locatedTable handlingDataDescripion db table name columns engine partitionBy orderBy .
  ( HasChSchema handlingDataDescripion
  , KnownTupleSymbols (ValidatedRequestedColumns (Unwraped handlingDataDescripion) columns)
  , IsTable table name columns engine partitionBy orderBy
  , KnownSymbol db, locatedTable ~ InDatabase db table
  ) => HttpChClient -> [handlingDataDescripion] -> IO (H.Response BSL.ByteString)
httpStreamChInsert (HttpChClient man req) schemaList = do
  resp <- H.httpLbs
    req
      { requestBody = H.requestBodySourceChunked $
        yield     (encodeUtf8 $ tsvInsertQueryHeader @handlingDataDescripion @locatedTable)
        >> yieldMany (map toBs schemaList)
      }
    man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure resp


tsvInsertQueryHeader :: forall handlingDataDescripion t db name columns engine partitionBy orderBy .
  ( HasChSchema handlingDataDescripion
  , KnownTupleSymbols (ValidatedRequestedColumns (Unwraped handlingDataDescripion) columns)
  , t ~ InDatabase db (Table name columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  ) => Text
tsvInsertQueryHeader =
  let columnsMapping = T.intercalate "," . map fst $ symbolsTupleVals @(ValidatedRequestedColumns (Unwraped handlingDataDescripion) columns)
  in "INSERT INTO " <> (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name)
  <> " (" <> columnsMapping <> ")"
  <> " FORMAT TSV\n"
{-# INLINE tsvInsertQueryHeader #-}

class HasChSchema a where
  default toBs :: (Generic a, GToBs (Rep a)) => a -> BS.ByteString
  toBs :: a -> BS.ByteString
  toBs = (<> "\n") . gToBs . from
  {-# INLINE toBs #-}

  default fromBs :: (Generic a, GFromBS (Rep a)) => BS.ByteString -> a
  fromBs :: BS.ByteString -> a
  fromBs = to . gFromBs
  {-# INLINE fromBs #-}




class GToBs f where
  gToBs :: f p -> BS.ByteString

instance GToBs f
  => GToBs (D1 c f) where
  gToBs (M1 re) = gToBs re
  {-# INLINE gToBs #-}
instance GToBs f
  => GToBs (C1 c f) where
  gToBs (M1 re) = gToBs re
  {-# INLINE gToBs #-}
instance (GToBs f, GToBs f2)
  => GToBs (f :*: f2) where
  gToBs (f :*: f2) = gToBs f <> "\t" <> gToBs f2
  {-# INLINE gToBs #-}
instance (IsChType p)
  => GToBs (S1 s (K1 i p)) where
  gToBs (M1 re) = render $ unK1 re
  {-# INLINE gToBs #-}




class GFromBS f where
  gFromBs :: BS.ByteString -> f p

instance GFromBS f => GFromBS (D1 c f) where
  gFromBs bs = M1 $ gFromBs bs
  {-# INLINE gFromBs #-}
instance GFromBS f => GFromBS (C1 c f) where
  gFromBs bs = M1 $ gFromBs bs
  {-# INLINE gFromBs #-}
instance (IsChType p)
  => GFromBS (S1 s (K1 i p)) where
  gFromBs bs = M1 $ K1 $ parse bs
  {-# INLINE gFromBs #-}
instance (GFromBS f1, GFromBS f2)
  => GFromBS (f1 :*: f2) where
  gFromBs bs =
    -- really need to optomize later
    let byteStrings = '\t' `split` bs
        lng = length byteStrings
        firstWordsCount = lng `div` 2
        lastWordsCount = lng - firstWordsCount
        firstWords = BS8.intercalate "\t" $ take firstWordsCount byteStrings
        lastWords = BS8.intercalate "\t" $ reverse $ take lastWordsCount $ reverse byteStrings in
    gFromBs firstWords
    :*:
    gFromBs lastWords
  {-# INLINE gFromBs #-}
