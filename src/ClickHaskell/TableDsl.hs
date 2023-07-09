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
  , InstanceSigs
  , OverloadedStrings
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , TypeApplications
  , ScopedTypeVariables
  , UndecidableInstances
#-}

{-# OPTIONS_GHC
  -Wno-missing-methods
#-}

module ClickHaskell.TableDsl
  ( InDatabase

  , Table, IsTable(..), IsLocatedTable(..)
  , DefaultColumn
  , OrderBy, PartitionBy, PrimaryKey

  , IsChEngine
  , MergeTree, TinyLog

  , ShowColumns
  , showCreateTableIfNotExists, showCreateTable, createDatabaseIfNotExists, createTableIfNotExists

  , KnownTupleSymbols(symbolsTupleVals)
  ) where

-- Internal dependencies
import ClickHaskell.Client  (ChException (..), HttpChClient (..))
import ClickHaskell.DbTypes (ToChTypeName)

-- External dependencies
import Network.HTTP.Client         as H (RequestBody (..), httpLbs, responseBody, responseStatus)
import Network.HTTP.Client.Conduit as H (Request (..))
import Network.HTTP.Types          as H (statusCode)

-- GHC included libraries imports
import Control.Exception     (throw)
import Control.Monad         (when)
import Data.ByteString       as BS (toStrict)
import Data.ByteString.Char8 as BS8 (fromStrict, pack)
import Data.Data             (Proxy (Proxy))
import Data.Kind             (Type)
import Data.Text             as T (Text, intercalate, pack)
import Data.Text.Encoding    as T (decodeUtf8, encodeUtf8)
import Data.Type.Bool        (If)
import Data.Type.Ord         (type (>?))
import GHC.TypeLits          (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


data OrderBy (columnNamesList :: [Symbol])
data PartitionBy (columnNamesList :: [Symbol])
data PrimaryKey (columnNamesList :: [Symbol])



class IsChEngine (GetTableEngine a) => HasTableEngine a where
  type GetTableEngine a :: Type

instance
  ( IsChEngine (GetTableEngine a)
  ) => HasTableEngine (InDatabase dbName a)
  where
  type GetTableEngine (InDatabase dbName a) = GetTableEngine a

instance
  ( IsChEngine engine
  ) => HasTableEngine (Table name columns engine engineSpecificSettings)
  where
  type GetTableEngine (Table name columns engine engineSpecificSettings) = engine




class
  IsLocatedTable table
  where
  getDatabaseName :: Text


instance {-# OVERLAPPING #-}
  ( KnownSymbol dbName
  , IsTable table
  ) => IsLocatedTable (InDatabase dbName table)
  where
  getDatabaseName = T.pack $ symbolVal (Proxy @dbName)

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "Expected a table description with its location. E. g. (InDatabase \"myDatabase\" MyTable)" :$$: 'Text " but got " :<>: ShowType table)
  ) => IsLocatedTable table
  where
  getDatabaseName = error "Unreachable"

instance {-# OVERLAPPABLE #-}
  ( tableName ~ ('Text "(Table \"" :<>: 'Text a :<>: 'Text "\" ...)")
  , TypeError
    (    'Text "Expected a table description with its location description. But got just Table"
    :$$: 'Text "Specify table location:"
    :$$: 'Text "  |(InDatabase \"yourDatabaseName\" " :<>: tableName :<>: 'Text ")"
    )
  ) => IsLocatedTable (Table a b c d)
  where
  getDatabaseName = error "Unreachable"





class IsTable table where
  type IsSupportedTable table :: Bool
  type GetTableColumns table :: [(Symbol, Type)]
  type GetTableName table :: Symbol
  type GetEngineSpecificSettings table :: [Type]

  getTableName :: Text


instance {-# OVERLAPS #-}
  ( KnownSymbol name
  ) => IsTable (Table name columns engine engineSpecificSettings)
  where
  type IsSupportedTable (Table _ _ _ _) = True
  type GetTableColumns (Table _ columns _ _) = DeduplicatedAndAlphabeticallySorted (TransformedToSupportedColumns columns)
  type GetTableName (Table name _ _ _) = name
  type GetEngineSpecificSettings (Table _ _ _ engineSpecificSettings) = engineSpecificSettings

  getTableName :: Text
  getTableName = (T.pack . symbolVal) (Proxy @name)

instance {-# OVERLAPS #-} (IsTable table) => IsTable (InDatabase db table)
  where
  type IsSupportedTable (InDatabase db table) = True
  type GetTableColumns (InDatabase db table) = GetTableColumns table
  type GetTableName (InDatabase db table) = GetTableName table
  type GetEngineSpecificSettings (InDatabase db table) = GetEngineSpecificSettings table

  getTableName :: Text
  getTableName = getTableName @table

instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a table description, but got " :<>: ShowType something
    :$$: 'Text "Provide a type that describe a table")
  ) => IsTable something




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


createTableIfNotExists :: forall locatedTable .
  ( IsLocatedTable locatedTable
  , IsTable locatedTable
  , HasTableEngine locatedTable
  , KnownSymbol (GetTableName locatedTable)
  , KnownTupleSymbols (ShowColumns (GetTableColumns locatedTable))
  ) => HttpChClient -> IO ()
createTableIfNotExists (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.RequestBodyLBS
      $ BS8.fromStrict . T.encodeUtf8 $ showCreateTableIfNotExists @locatedTable
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp




type family DeduplicatedAndAlphabeticallySorted xs :: [(Symbol, Type)] where
  DeduplicatedAndAlphabeticallySorted (fst ': snd ': xs) = HasNoDuplicationsAndAplabeticlySorted fst snd ': DeduplicatedAndAlphabeticallySorted (snd ': xs)
  DeduplicatedAndAlphabeticallySorted '[x] = '[x]
  DeduplicatedAndAlphabeticallySorted '[] = '[]

type family HasNoDuplicationsAndAplabeticlySorted firstColumn secondColumn :: (Symbol, Type) where
  HasNoDuplicationsAndAplabeticlySorted '(sym, type1)  '(sym, type2)  = TypeError ('Text "There are 2 columns with identical names: " :<>: 'Text sym :<>: 'Text ". Rename one of them")
  HasNoDuplicationsAndAplabeticlySorted '(sym1, type1) '(sym2, type2) =
    If
      (sym1 >? sym2)
      (TypeError
        (    'Text "There are some aplabetically unsorted columns. Column with name \"" :<>: 'Text sym1 :<>: 'Text "\" should be after " :<>: 'Text sym2 
        :$$: 'Text "Aplabetically sorting required to make compilation faster. Also it makes your code more readable"
        )
      )
      '(sym1, type1)




type family TransformedToSupportedColumns (columns :: [Type]) :: [(Symbol, Type)] where
  TransformedToSupportedColumns (x ': '[]) = SupportedColumn x ': '[]
  TransformedToSupportedColumns (x ': xs)  = SupportedColumn x ': TransformedToSupportedColumns xs
  TransformedToSupportedColumns '[]        = TypeError ('Text "No columns in table")


type family SupportedColumn x :: (Symbol, Type) where
  SupportedColumn (DefaultColumn a b) = '(a, b)


type family ShowColumns t :: [(Symbol, Symbol)] where
  ShowColumns ( '(a, b) ': xs) = '(a, ToChTypeName b) ': ShowColumns xs
  ShowColumns '[] = '[]




data InDatabase
  (db :: Symbol)
  (t :: Type)


data Table
  (name :: Symbol)
  (columns :: [Type])
  (engine :: Type)
  (engineSpecificSettings :: [Type])




class KnownTupleSymbols (ns :: [(Symbol, Symbol)]) where
  symbolsTupleVals :: [(Text, Text)]
instance KnownTupleSymbols '[] where symbolsTupleVals = []
instance (KnownTupleSymbols ns, KnownSymbol a, KnownSymbol b) => KnownTupleSymbols ('(a,b) ': ns) where
  symbolsTupleVals = (T.pack (symbolVal (Proxy :: Proxy a)), T.pack (symbolVal (Proxy :: Proxy b))) : symbolsTupleVals @ns




data DefaultColumn (name :: Symbol) columnType




showCreateTableIfNotExists :: forall locatedTable .
  ( IsLocatedTable locatedTable
  , IsTable locatedTable
  , HasTableEngine locatedTable
  , KnownSymbol (GetTableName locatedTable)
  , KnownTupleSymbols (ShowColumns (GetTableColumns locatedTable))
  ) => Text
showCreateTableIfNotExists =
  let columns     = symbolsTupleVals @(ShowColumns (GetTableColumns locatedTable))
  in "CREATE TABLE IF NOT EXISTS "  <> getDatabaseName @locatedTable <> "." <> getTableName @locatedTable
  <> " "              <> ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> engineName @(GetTableEngine locatedTable)
  <> " PARTITION BY " <> "tuple()"
  <> " ORDER BY "     <> "tuple()"


showCreateTable :: forall locatedTable .
  ( IsLocatedTable locatedTable
  , IsTable locatedTable
  , HasTableEngine locatedTable
  , KnownSymbol (GetTableName locatedTable)
  , KnownTupleSymbols (ShowColumns (GetTableColumns locatedTable))
  ) => Text
showCreateTable =
  let columns     = symbolsTupleVals @(ShowColumns (GetTableColumns locatedTable))
  in "CREATE TABLE "  <> getDatabaseName @locatedTable <> "." <> getTableName @locatedTable
  <> " "              <> ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> engineName @(GetTableEngine locatedTable)
  <> " PARTITION BY " <> "tuple()"
  <> " ORDER BY "     <> "tuple()"




class    IsChEngine engine    where engineName :: Text
instance IsChEngine MergeTree where engineName = "MergeTree"
instance IsChEngine TinyLog   where engineName = "TinyLog"
instance {-# OVERLAPPABLE #-} TypeError
  (     'Text "Unknown table engine " ':<>: 'ShowType a
  ':$$: 'Text "Use one of the provided:"
  ':$$: 'Text "  MergeTree"
  ':$$: 'Text "  TinyLog"
  ':$$: 'Text "or implement your own support"
  )  => IsChEngine a where engineName = error "Unreachable"
data MergeTree
data TinyLog
