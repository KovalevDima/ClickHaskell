{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DerivingStrategies
  , FlexibleInstances
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , TypeApplications
  , ScopedTypeVariables
  , UndecidableInstances
#-}

module ClickHaskell.TableDsl
  ( InDatabase

  , Table, IsTable
  , DefaultColumn

  , IsChEngine
  , MergeTree, TinyLog

  , SupportedAndVerifiedColumns, ShowColumns
  , showCreateTableIfNotExists, showCreateTable, createDatabaseIfNotExists, createTableIfNotExists

  , KnownTupleSymbols(symbolsTupleVals)
  ) where

-- Internal dependencies
import ClickHaskell.Client           (HttpChClient(..), ChException (..))
import ClickHaskell.TableDsl.DbTypes (ToChTypeName)

-- External dependencies
import Network.HTTP.Client         as H (httpLbs, responseStatus, responseBody, RequestBody(..))
import Network.HTTP.Client.Conduit as H (Request(..))
import Network.HTTP.Types          as H (statusCode)

-- GHC included libraries imports
import Control.Exception     (throw)
import Control.Monad         (when)
import Data.ByteString       as BS (toStrict)
import Data.ByteString.Char8 as BS8 (pack, fromStrict)
import Data.Data             (Proxy(Proxy))
import Data.Text.Encoding    as T (decodeUtf8)
import Data.Kind             (Type)
import Data.Text             as T (Text, pack, unpack, intercalate)
import GHC.TypeLits          (symbolVal, KnownSymbol, TypeError, ErrorMessage(..), Symbol)


createDatabaseIfNotExists :: forall db . KnownSymbol db => HttpChClient -> IO ()
createDatabaseIfNotExists (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.RequestBodyLBS
      $ "CREATE DATABASE IF NOT EXISTS " <> (BS8.fromStrict . BS8.pack. symbolVal) (Proxy @db)
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp


createTableIfNotExists :: forall locatedTable db table name columns engine orderBy partitionBy .
  ( IsTable table name columns engine orderBy partitionBy
  , locatedTable ~ InDatabase db table
  , KnownSymbol db
  ) => HttpChClient -> IO ()
createTableIfNotExists (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.RequestBodyLBS
      $ BS8.fromStrict . BS8.pack $ showCreateTableIfNotExists @locatedTable
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp




type family SupportedAndVerifiedColumns (columns :: [Type]) :: [(Symbol, Type)] where
  SupportedAndVerifiedColumns xs = NoDuplicated (TransformedToSupportedColumns xs)

type NoDuplicated :: [(Symbol, Type)] -> [(Symbol, Type)]
type family NoDuplicated xs where
  NoDuplicated (x ': xs) = ElemOrNot x xs ': NoDuplicated xs
  NoDuplicated '[] = '[]

type ElemOrNot :: a -> [a] -> a
type family ElemOrNot a as where
  ElemOrNot a '[] = a
  ElemOrNot '(a, _) ('(a, _) ': xs) = 
    TypeError ('Text "There is a field " ':<>: 'Text a ':<>: 'Text " duplicated")
  ElemOrNot '(a, c) ('(b, _) ': xs) = ElemOrNot '(a, c) xs




type family TransformedToSupportedColumns (columns :: [Type]) :: [(Symbol, Type)] where
  TransformedToSupportedColumns (x ': '[]) = SupportedColumn x ': '[]
  TransformedToSupportedColumns (x ': xs)  = SupportedColumn x ': TransformedToSupportedColumns xs
  TransformedToSupportedColumns '[]        = TypeError ('Text "No columns in table")


type family SupportedColumn x :: (Symbol, Type) where
  SupportedColumn (DefaultColumn a b) = '(a, b)


type ShowColumns :: [(Symbol, Type)] -> [(Symbol, Symbol)] 
type family ShowColumns t where
  ShowColumns ( '(a, b) ': xs) = '(a, ToChTypeName b) ': ShowColumns xs
  ShowColumns '[] = '[]




data InDatabase
  (db :: Symbol)
  (t :: Type)


data Table
  (name :: Symbol)
  (columns :: [column :: Type])
  engine
  (partitionBy :: [Symbol])
  (orderBy     :: [Symbol])

type IsTable t name columns engine orderBy partitionBy =
  ( t ~ Table name columns engine orderBy partitionBy
  , KnownSymbol name
  , KnownSymbols partitionBy
  , KnownSymbols orderBy
  , KnownTupleSymbols (ShowColumns (SupportedAndVerifiedColumns columns))
  , IsChEngine engine
  )


class KnownSymbols (ns :: [Symbol]) where symbolsVal :: [Text]
instance KnownSymbols '[] where symbolsVal = []
instance (KnownSymbol n, KnownSymbols ns) => KnownSymbols (n ': ns) where
  symbolsVal = T.pack (symbolVal (Proxy :: Proxy n)) : symbolsVal @ns

class KnownTupleSymbols (ns :: [(Symbol, Symbol)]) where
  symbolsTupleVals :: [(Text, Text)]
instance KnownTupleSymbols '[] where symbolsTupleVals = []
instance (KnownSymbol a, KnownSymbol b, KnownTupleSymbols ns) => KnownTupleSymbols ('(a,b) ': ns) where
  symbolsTupleVals = (T.pack (symbolVal (Proxy :: Proxy a)), T.pack (symbolVal (Proxy :: Proxy b))) : symbolsTupleVals @ns



data DefaultColumn (name :: Symbol) columnType




showCreateTableIfNotExists :: forall t db table name columns engine orderBy partitionBy .
  ( IsTable table name columns engine partitionBy orderBy
  , KnownSymbol db, t ~ InDatabase db table
  ) => String
showCreateTableIfNotExists =
  let columns     = symbolsTupleVals @(ShowColumns (SupportedAndVerifiedColumns columns))
      partitionBy = symbolsVal @partitionBy
      orderBy     = symbolsVal @orderBy
  in "CREATE TABLE IF NOT EXISTS "  <> symbolVal (Proxy @db) <> "." <> symbolVal (Proxy @name)
  <> " "              <> T.unpack ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> engineName @engine
  <> " PARTITION BY " <> (if null partitionBy then "tuple()" else T.unpack ("(" <> T.intercalate ", " partitionBy <> ")"))
  <> " ORDER BY "     <> (if null orderBy     then "tuple()" else T.unpack ("(" <> T.intercalate ", " orderBy     <> ")"))


showCreateTable :: forall t db table name columns engine orderBy partitionBy .
  ( IsTable table name columns engine partitionBy orderBy
  , KnownSymbol db, t ~ InDatabase db table
  ) => String
showCreateTable =
  let columns     = symbolsTupleVals @(ShowColumns (SupportedAndVerifiedColumns columns))
      partitionBy = symbolsVal @partitionBy
      orderBy     = symbolsVal @orderBy
  in "CREATE TABLE "  <> symbolVal (Proxy @db) <> "." <> symbolVal (Proxy @name)
  <> " "              <> T.unpack ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> engineName @engine
  <> " PARTITION BY " <> (if null partitionBy then "tuple()" else T.unpack ("(" <> T.intercalate ", " partitionBy <> ")"))
  <> " ORDER BY "     <> (if null orderBy     then "tuple()" else T.unpack ("(" <> T.intercalate ", " orderBy     <> ")"))




class    IsChEngine engine    where engineName :: String
instance IsChEngine MergeTree where engineName = "MergeTree"
instance IsChEngine TinyLog   where engineName = "TinyLog"
instance {-# OVERLAPPABLE #-} TypeError
  (     'Text "Unknown table engine " ':<>: 'ShowType a
  ':$$: 'Text "Use one of the following:"
  ':$$: 'Text "  MergeTree"
  ':$$: 'Text "  TinyLog"
  ':$$: 'Text "or implement your own support"
  )  => IsChEngine a where engineName = error "Unsupported engine"
data TinyLog
data MergeTree
