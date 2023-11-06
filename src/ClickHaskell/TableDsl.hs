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
  ( IsTable(..), Table
  , ExpectsFiltrationBy

  , IsColumnDescription(..)
  , DefaultColumn, ReadOnlyColumn
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes (ToChTypeName, IsChType)

-- GHC included libraries imports
import Data.Data      (Proxy (Proxy))
import Data.Kind      (Type)
import Data.Text      as T (Text, pack)
import Data.Type.Bool (If)
import Data.Type.Ord  (type(>?))
import GHC.TypeLits   (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


data ExpectsFiltrationBy (columnNamesList :: [Symbol])




class IsTable table where
  type GetTableColumns table :: [(Symbol, Type)]
  type GetTableName table :: Symbol
  type GetTableSettings table :: [Type]
  type GetTableEngine table :: Type
  type TableValidationResult table :: (Bool, ErrorMessage)

  getTableName :: Text
  getTableRenderedColumns :: [(Text, Text)]

  getTableRenderedColumnsNames :: [Text]
  getTableRenderedColumnsNames = fst `map` getTableRenderedColumns @table

data Table
  (name :: Symbol)
  (columns :: [Type])
  (settings :: [Type])


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a valid table description, but got: "
    :$$: ShowType something
    )
  ) => IsTable something


instance {-# OVERLAPS #-}
  ( KnownSymbol name
  , KnownTupleSymbols (ShowColumns (TransformedToSupportedColumns columns))
  ) => IsTable (Table name columns settings)
  where
  type GetTableColumns (Table _ columns _) = TransformedToSupportedColumns columns
  type GetTableName (Table name _ _) = name
  type GetTableSettings (Table _ _ settings) = settings
  type TableValidationResult (Table _ columns _) = IsValidColumnsDescription (TransformedToSupportedColumns columns)

  getTableName :: Text
  getTableName = (T.pack . symbolVal) (Proxy @name)

  getTableRenderedColumns :: [(Text, Text)]
  getTableRenderedColumns = symbolsTupleVals @(ShowColumns (TransformedToSupportedColumns columns))




type family IsValidColumnsDescription xs :: (Bool, ErrorMessage) where
  IsValidColumnsDescription ('(sameName, _) ': '(sameName, _) ': _) = '(True, 'Text "There are 2 columns with identical names: " :<>: 'Text sameName :<>: 'Text "\". Rename one of them")
  IsValidColumnsDescription '[x] = '(False, 'Text "Report an issue if you see this message [ClickHaskell.TableDsl.1]")
  IsValidColumnsDescription '[] = '(True, 'Text "Data source should have at least one column but given empty list of expected columns")
  IsValidColumnsDescription ('(name1, type1) ': '(name2, type2) ': columns)
    = If (name1 >? name2)
      '( True
       , 'Text "Table columns description contains aplabetically unsorted columns."
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


class KnownTupleSymbols (ns :: [(Symbol, Symbol)]) where
  symbolsTupleVals :: [(Text, Text)]
instance KnownTupleSymbols '[] where symbolsTupleVals = []
instance (KnownTupleSymbols ns, KnownSymbol a, KnownSymbol b) => KnownTupleSymbols ('(a,b) ': ns) where
  symbolsTupleVals = (T.pack (symbolVal (Proxy @a)), T.pack (symbolVal (Proxy @b))) : symbolsTupleVals @ns




class IsColumnDescription columnDescription where
  type GetColumnName columnDescription :: Symbol
  renderColumnName :: Text

  type GetColumnType columnDescription :: Type
  renderColumnType :: Text

  type IsColumnReadOnly columnDescription :: Bool

data DefaultColumn (name :: Symbol) (columnType :: Type)
data ReadOnlyColumn (name :: Symbol) (columnType :: Type)


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (   'Text "Expected a valid column description. But got: "
    :$$: ShowType unsupportedColumnDescription
    )
  ) => IsColumnDescription unsupportedColumnDescription


instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => IsColumnDescription (DefaultColumn name columnType)
  where
  type GetColumnName (DefaultColumn name columnType) = name
  renderColumnName = T.pack . symbolVal $ Proxy @name

  type GetColumnType (DefaultColumn name columnType) = columnType
  renderColumnType = T.pack . symbolVal $ Proxy @(ToChTypeName columnType)

  type IsColumnReadOnly (DefaultColumn _ _) = False


instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => IsColumnDescription (ReadOnlyColumn name columnType)
  where
  type GetColumnName (ReadOnlyColumn name _) = name
  renderColumnName = T.pack . symbolVal $ Proxy @name

  type GetColumnType (ReadOnlyColumn _ columnType) = columnType
  renderColumnType = T.pack . symbolVal $ Proxy @(ToChTypeName columnType)

  type IsColumnReadOnly (ReadOnlyColumn _ _) = True
