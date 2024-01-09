{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , InstanceSigs
  , OverloadedStrings
  , UndecidableInstances
#-}

{-# OPTIONS_GHC
  -Wno-missing-methods
#-}

module ClickHaskell.Tables
  ( IsTable(..)
  , Table
  , View, Parameter

  , IsColumnDescription(..)
  , Column, ReadOnlyColumn
  ) where

-- Internal dependencies
import ClickHouse.DbTypes (IsChType (..))

-- GHC included libraries imports
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type, Constraint)
import Data.Text               as T (Text, pack)
import Data.Type.Bool          (If)
import Data.Type.Equality      (type(==))
import Data.Type.Ord           (type(>?))
import GHC.TypeLits            (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


class
  IsTable table where
  type GetTableName table :: Symbol
  type GetTableColumns table :: [Type]
  type GetTableSettings table :: [Type]
  type ValidateTable table :: Maybe ErrorMessage
  type Writable table :: Constraint

  mkTableName :: Builder


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a valid table description, but got:"
    :$$: ShowType something
    )
  ) => IsTable something


data Table
  (name :: Symbol)
  (columns :: [Type])
  (settings :: [Type])


data View
  (name :: Symbol)
  (columns :: [Type])
  (parameters :: [Type])
  (settings :: [Type])

data Parameter (name :: Symbol) (chType :: Type)


instance
  ( KnownSymbol name
  , IsValidColumns columns
  ) => IsTable (Table name columns settings)
  where
  type GetTableName     (Table name _ _)     = name
  type GetTableColumns  (Table _ columns _)  = GetColumnsRep columns
  type GetTableSettings (Table _ _ settings) = settings
  type ValidateTable    (Table _ columns _)  = ColumnsValdationResult columns
  type Writable         (Table _ _ _)        = ()

  mkTableName :: Builder
  mkTableName =  "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""


instance
  ( KnownSymbol name
  , IsValidColumns columns
  ) => IsTable (View name columns parameters settings)
  where
  type GetTableName     (View name _ _ _)     = name
  type GetTableColumns  (View _ columns _ _)  = GetColumnsRep columns
  type GetTableSettings (View _ _ _ settings) = settings
  type ValidateTable    (View _ columns _ _)  = ColumnsValdationResult columns
  type Writable         (View _ _ _ _)        = TypeError
    (    'Text "You can only Reading data from View"
    :$$: 'Text "But you are trying Writing into it"
    )

  mkTableName :: Builder
  mkTableName = "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""








class IsValidColumns (columns :: [Type])
  where
  type GetColumnsRep columns :: [Type]
  getRenederedColumns :: [(Text, Text)]
  
  type ColumnsValdationResult columns :: Maybe ErrorMessage


instance
  ( TypeError
    (    'Text "Data source should have at least one column"
    :$$: 'Text "But given empty list of expected columns"
    )
  ) => IsValidColumns '[]


instance
  ( IsColumnDescription column1
  , IsColumnDescription column2
  , IsValidColumns (column2 ': columns)
  ) => IsValidColumns (column1 ': column2 ': columns)
  where
  type GetColumnsRep (column1 ': column2 ': columns) = column1 ': GetColumnsRep (column2 ': columns)
  getRenederedColumns = (renderColumnName @column1, renderColumnType @column2) : getRenederedColumns @(column2 ': columns)

  type (ColumnsValdationResult (column1 ': column2 ': columns)) =
    If (GetColumnName column1 >? GetColumnName column2)
      ( TypeError
       (     'Text "Table columns description contains aplabetically unsorted columns."
        :$$: 'Text "Column with name \""         :<>: 'Text (GetColumnName column1) :<>: 'Text "\" "
          :<>: 'Text "should be placed after \"" :<>: 'Text (GetColumnName column2) :<>: 'Text "\""
       ) 
      )
      (If (GetColumnName column2 == GetColumnName column1)
        (TypeError ('Text "There are two columns with identical name: \"" :<>: 'Text (GetColumnName column1) :<>: 'Text "\""))
        (ColumnsValdationResult (column2 ': columns))
      )


instance
  ( IsColumnDescription column
  ) => IsValidColumns '[column]
  where
  type GetColumnsRep '[column] = '[column]
  getRenederedColumns = [(renderColumnName @column, renderColumnType @column)]

  type (ColumnsValdationResult '[column]) = 'Nothing








class IsColumnDescription columnDescription where
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


data Column         (name :: Symbol) (columnType :: Type)
data ReadOnlyColumn (name :: Symbol) (columnType :: Type)


instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => IsColumnDescription (Column name columnType)
  where
  type GetColumnName (Column name columnType) = name
  renderColumnName = T.pack . symbolVal $ Proxy @name

  type GetColumnType (Column name columnType) = columnType
  renderColumnType = T.pack . symbolVal $ Proxy @(ToChTypeName columnType)

  type IsColumnReadOnly      (Column _ _)          = 'False
  type IsColumnWriteOptional (Column _ columnType) = IsWriteOptional columnType


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

  type IsColumnReadOnly      (ReadOnlyColumn _ _) = True
  type IsColumnWriteOptional (ReadOnlyColumn _ _) = True
