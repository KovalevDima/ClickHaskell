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
module ClickHaskell.DataManipulationDsl where


-- Internal dependencies
import ClickHaskell.Client
import ClickHaskell.TableDsl

-- External dependenices
import Conduit              (yield, yieldMany)
import Network.HTTP.Types   as H
import Network.HTTP.Conduit as H hiding (Proxy)

-- GHC included libraries imports
import Control.Exception          (throw)
import Control.Monad              (when)
import Data.ByteString            as BS (toStrict)
import Data.ByteString.Lazy       as BSL (ByteString, toStrict)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.Data                  (Proxy (..))
import Data.Text                  as T (Text, intercalate, pack)
import Data.Text.Encoding         as T (decodeUtf8, encodeUtf8)
import Data.Text.Lazy             as T (toStrict)
import Data.Text.Lazy.Builder     as T (toLazyText)
import GHC.TypeLits               (KnownSymbol, symbolVal)



httpStreamChSelect :: forall handlingDataDescripion locatedTable db table name columns engine partitionBy orderBy .
  ( HasChSchema (Unwraped handlingDataDescripion)
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

-- ToDo4: Implement table and handling data validation
tsvSelectQuery :: forall
  handlingDataDescripion tableWithDb
  db
  table name columns engine partitionBy orderBy
  .
  ( HasChSchema (Unwraped handlingDataDescripion)
  , ToConditionalExpression handlingDataDescripion
  , IsTable table name columns engine partitionBy orderBy
  , tableWithDb ~ InDatabase db table
  , KnownSymbol db
  ) => Text
tsvSelectQuery =
  let columnsMapping = T.intercalate "," . map fst $ getSchema @(Unwraped handlingDataDescripion)
      whereConditions = T.toStrict (T.toLazyText $ toConditionalExpression @handlingDataDescripion)
  in  "SELECT " <> columnsMapping
  <> " FROM " <> (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name)
  <> " " <> (if whereConditions=="" then "" else "WHERE " <> whereConditions)
  <> " FORMAT TSV"
{-# INLINE tsvSelectQuery #-}


httpStreamChInsert :: forall locatedTable handlingDataDescripion db table name columns engine partitionBy orderBy .
  ( HasChSchema handlingDataDescripion
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


-- ToDo4: Implement table and handling data validation
tsvInsertQueryHeader :: forall chSchema t db table name columns engine partitionBy orderBy .
  ( HasChSchema chSchema
  , t ~ InDatabase db (table name columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  ) => Text
tsvInsertQueryHeader =
  let columnsMapping = T.intercalate "," . map fst $ getSchema @chSchema
  in "INSERT INTO " <> (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name)
  <> " (" <> columnsMapping <> ")"
  <> " FORMAT TSV\n"
{-# INLINE tsvInsertQueryHeader #-}
