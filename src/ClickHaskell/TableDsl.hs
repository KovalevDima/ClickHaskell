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
import ClickHaskell.DbTypes (IsChType (..))

-- GHC included libraries imports
import Data.Data          (Proxy (Proxy))
import Data.Kind          (Type)
import Data.Text          as T (Text, pack)
import Data.Type.Bool     (If)
import Data.Type.Equality (type(==))
import Data.Type.Ord      (type(>?))
import GHC.TypeLits       (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


data ExpectsFiltrationBy (columnNamesList :: [Symbol])




data Table
  (name :: Symbol)
  (columns :: [Type])
  (settings :: [Type])

class IsTable table where
  type GetTableName table :: Symbol
  type GetTableColumns table :: [Type]
  type GetTableSettings table :: [Type]
  type TableValidationResult table :: Maybe ErrorMessage

  getTableName :: Text
  getTableRenderedColumns :: [(Text, Text)]

  getTableRenderedColumnsNames :: [Text]
  getTableRenderedColumnsNames = fst `map` getTableRenderedColumns @table


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a valid table description, but got: "
    :$$: ShowType something
    )
  ) => IsTable something


instance {-# OVERLAPS #-}
  ( KnownSymbol name
  , IsValidColumns columns
  ) => IsTable (Table name columns settings)
  where
  type GetTableName     (Table name _ _)     = name
  type GetTableColumns  (Table _ columns _)  = GetColumnsRep columns
  type GetTableSettings (Table _ _ settings) = settings
  type TableValidationResult (Table _ columns _) = ColumnsValdationResult columns

  getTableName :: Text
  getTableName = (T.pack . symbolVal) (Proxy @name)

  getTableRenderedColumns :: [(Text, Text)]
  getTableRenderedColumns = getRenederedColumns @columns




class IsValidColumns (columns :: [Type])
  where
  type ColumnsValdationResult columns :: Maybe ErrorMessage
  type GetColumnsRep columns :: [Type]
  getRenederedColumns :: [(Text, Text)]


instance
  ( IsColumnDescription column1
  , IsColumnDescription column2
  , IsValidColumns (column2 ': columns)
  ) => IsValidColumns (column1 ': column2 ': columns)
  where
  type GetColumnsRep (column1 ': column2 ': columns) = column1 ': GetColumnsRep (column2 ': columns)
  type (ColumnsValdationResult (column1 ': column2 ': columns)) =
    If (GetColumnName column1 >? GetColumnName column2)
      ( 'Just
       (     'Text "Table columns description contains aplabetically unsorted columns."
        :$$: 'Text "Column with name \""         :<>: 'Text (GetColumnName column1) :<>: 'Text "\" "
          :<>: 'Text "should be placed after \"" :<>: 'Text (GetColumnName column2) :<>: 'Text "\""
       ) 
      )
      (If (GetColumnName column2 == GetColumnName column1)
        ('Just ('Text "There are two columns with identical name: \"" :<>: 'Text (GetColumnName column1) :<>: 'Text "\""))
        (ColumnsValdationResult (column2 ': columns))
      )
  getRenederedColumns = (renderColumnName @column1, renderColumnType @column2) : getRenederedColumns @(column2 ': columns)


instance
  ( IsColumnDescription column
  ) => IsValidColumns '[column]
  where
  type GetColumnsRep '[column] = '[column]
  getRenederedColumns = [(renderColumnName @column, renderColumnType @column)]

  type (ColumnsValdationResult '[column]) = 'Nothing


instance
  ( TypeError ('Text "Data source should have at least one column but given empty list of expected columns")
  ) => IsValidColumns '[]




data DefaultColumn (name :: Symbol) (columnType :: Type)
data ReadOnlyColumn (name :: Symbol) (columnType :: Type)


class IsColumnDescription columnDescription where
  type ColumnDescriptionValidationResult columnDescription :: Maybe ErrorMessage
  type GetColumnName columnDescription :: Symbol
  renderColumnName :: Text

  type GetColumnType columnDescription :: Type
  renderColumnType :: Text

  type IsColumnReadOnly columnDescription :: Bool
  type IsColumnWriteOptional columnDescription :: Bool


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a valid column description. But got: "
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

  type IsColumnReadOnly (DefaultColumn _ columnType) = 'False
  type IsColumnWriteOptional (DefaultColumn _ columnType) = IsWriteOptional columnType


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
  type IsColumnWriteOptional (ReadOnlyColumn _ columnType) = 'True




-- * Errors handling

-- | Type family usefull when you have a several posible errors and need to return first one.
-- 
-- @
-- type NotAnError = 'Nothng
-- type Error1     = 'Just ('Text "error1")
-- type Error2     = 'Just ('Text "error2")
--
-- type FirstJustOrNothing '[NotAnError, Error2] = 'Just ('Text "error2")
-- type FirstJustOrNothing '[Error1,     Error2] = 'Just ('Text "error1")
-- type FirstJustOrNothing '[NotAnError]         = 'Nothing
-- @
type family FirstJustOrNothing (a :: [Maybe ErrorMessage]) :: Maybe ErrorMessage
  where
  FirstJustOrNothing '[] = 'Nothing
  FirstJustOrNothing ('Nothing ': xs) = FirstJustOrNothing xs
  FirstJustOrNothing ('Just txt ': xs) = 'Just txt
