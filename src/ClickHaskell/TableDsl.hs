{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleInstances
  , FlexibleContexts
  , InstanceSigs
  , OverloadedStrings
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
  ( IsLocatedTable(..), InDatabase

  , IsTable(..), Table
  , OrderBy, PartitionBy, PrimaryKey
  , DefaultColumn

  , IsChEngine
  , MergeTree

  , showCreateTableIfNotExists, showCreateTable
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes       (ToChTypeName)

-- GHC included libraries imports
import Data.Data      (Proxy (Proxy))
import Data.Kind      (Type)
import Data.Text      as T (Text, intercalate, pack)
import Data.Type.Bool (If)
import Data.Type.Ord  (type(>?))
import GHC.TypeLits   (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)

import ClickHaskell.Validation (HandleErrors)


data OrderBy (columnNamesList :: [Symbol])
data PartitionBy (columnNamesList :: [Symbol])
data PrimaryKey (columnNamesList :: [Symbol])




class
  IsLocatedTable table
  where
  type GetTable table :: Type
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
  type GetTableColumns table :: [(Symbol, Type)]
  type GetTableName table :: Symbol
  type GetEngineSpecificSettings table :: [Type]
  type GetTableEngine table :: Type
  type TableValidationResult table :: (Bool, ErrorMessage)

  getTableEngineName :: Text
  getTableName :: Text
  getTableRenderedColumnsNames :: [Text]
  getTableRenderedColumns :: [(Text, Text)]


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a table description, but got " :<>: ShowType something
    :$$: 'Text "Provide a type that describes a table")
  ) => IsTable something


instance {-# OVERLAPS #-} (IsTable table) => IsTable (InDatabase db table)
  where
  type GetTableColumns (InDatabase db table) = GetTableColumns table
  type GetTableName (InDatabase db table) = GetTableName table
  type GetTableEngine (InDatabase db table) = GetTableEngine table
  type GetEngineSpecificSettings (InDatabase db table) = GetEngineSpecificSettings table
  type TableValidationResult (InDatabase db table) = TableValidationResult table 

  getTableEngineName :: Text
  getTableEngineName = getTableEngineName @table

  getTableName :: Text
  getTableName = getTableName @table

  getTableRenderedColumnsNames :: [Text]
  getTableRenderedColumnsNames = getTableRenderedColumnsNames @table

  getTableRenderedColumns :: [(Text, Text)]
  getTableRenderedColumns = getTableRenderedColumns @table




instance {-# OVERLAPS #-}
  ( KnownSymbol name
  , IsChEngine engine
  , KnownTupleSymbols (ShowColumns (TransformedToSupportedColumns columns))
  ) => IsTable (Table name columns engine engineSpecificSettings)
  where
  type GetTableColumns (Table _ columns _ _) = TransformedToSupportedColumns columns
  type GetTableName (Table name _ _ _) = name
  type GetTableEngine (Table _ _ engine _) = engine
  type GetEngineSpecificSettings (Table _ _ _ engineSpecificSettings) = engineSpecificSettings
  type TableValidationResult (Table _ columns _ _) =
    HandleErrors
      '[ IsValidColumnsDescription (TransformedToSupportedColumns columns)
       ]

  getTableEngineName :: Text
  getTableEngineName = engineName @engine

  getTableName :: Text
  getTableName = (T.pack . symbolVal) (Proxy @name)

  getTableRenderedColumnsNames :: [Text]
  getTableRenderedColumnsNames = map fst $ symbolsTupleVals @(ShowColumns (TransformedToSupportedColumns columns))

  getTableRenderedColumns :: [(Text, Text)]
  getTableRenderedColumns = symbolsTupleVals @(ShowColumns (TransformedToSupportedColumns columns))




type family IsValidColumnsDescription xs :: (Bool, ErrorMessage) where
  IsValidColumnsDescription ('(sameName, type1) ': '(sameName, type2) ': xs) = '(True, 'Text "There are 2 columns with identical names: " :<>: 'Text sameName :<>: 'Text "\". Rename one of them")
  IsValidColumnsDescription '[x] = '(False, 'Text "Report an issue if you see this message [ClickHaskell.TableDsl.1]")
  IsValidColumnsDescription '[] = '(True, 'Text "Data source should have at least one column but given empty list of expected columns")
  IsValidColumnsDescription ('(name1, type1) ': '(name2, type2) ': columns)
    = If (name1 >? name2)
      '( True
       , 'Text "Source columns description contains aplabetically unsorted columns."
          :$$: 'Text "Column with name \"" :<>: 'Text name1 :<>: 'Text "\" should be placed after \"" :<>: 'Text name2 :<>: 'Text "\""
       )
      (IsValidColumnsDescription ('(name2, type2) ': columns))




type family TransformedToSupportedColumns (columns :: [Type]) :: [(Symbol, Type)] where
  TransformedToSupportedColumns (x ': '[]) = SupportedColumn x ': '[]
  TransformedToSupportedColumns (x ': xs)  = SupportedColumn x ': TransformedToSupportedColumns xs
  TransformedToSupportedColumns '[]        = TypeError ('Text "Report an issue if you see this message [ClickHaskell.TableDsl.2]")


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
  ) => Text
showCreateTableIfNotExists =
  let columns     = getTableRenderedColumns @locatedTable
  in "CREATE TABLE IF NOT EXISTS "  <> getDatabaseName @locatedTable <> "." <> getTableName @locatedTable
  <> " "              <> ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> getTableEngineName @locatedTable
  <> " PARTITION BY " <> "tuple()"
  <> " ORDER BY "     <> "tuple()"


showCreateTable :: forall locatedTable .
  ( IsLocatedTable locatedTable
  , IsTable locatedTable
  ) => Text
showCreateTable =
  let columns     = getTableRenderedColumns @locatedTable
  in "CREATE TABLE "  <> getDatabaseName @locatedTable <> "." <> getTableName @locatedTable
  <> " "              <> ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> getTableEngineName @locatedTable
  <> " PARTITION BY " <> "tuple()"
  <> " ORDER BY "     <> "tuple()"




class    IsChEngine engine    where engineName :: Text
instance IsChEngine MergeTree where engineName = "MergeTree"
instance {-# OVERLAPPABLE #-} TypeError
  (     'Text "Unknown table engine " ':<>: 'ShowType a
  ':$$: 'Text "Use one of the provided:"
  ':$$: 'Text "  MergeTree"
  ':$$: 'Text "or implement your own support"
  )  => IsChEngine a where engineName = error "Unreachable"

data MergeTree
