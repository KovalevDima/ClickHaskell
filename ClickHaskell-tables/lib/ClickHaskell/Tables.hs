{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , InstanceSigs
  , OverloadedStrings
  , NamedFieldPuns
  , UndecidableInstances
#-}

{-# OPTIONS_GHC
  -Wno-missing-methods
#-} 

module ClickHaskell.Tables
  ( TableInterpretable(..)
  , Parameter

  , Table, renderTable
  , View, renderView

  , IsColumnDescription(..)
  , Column, ReadOnlyColumn
  ) where


-- Internal
import ClickHouse.DbTypes (IsChType (..))


-- GHC included
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type)
import Data.Text               as T (Text, pack)
import Data.Type.Bool          (If)
import Data.Type.Equality      (type(==))
import Data.Type.Ord           (type(>?))
import GHC.TypeLits            (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


class
  TableInterpretable table
  where
  type GetTableName table :: Symbol
  type GetTableColumns table :: [Type]
  type ValidateTable table :: Maybe ErrorMessage

  type TableInterpreter table :: Type
  interpretTable :: TableInterpreter table


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a valid table description, but got:"
    :$$: ShowType something
    )
  ) => TableInterpretable something




-- ** Table

newtype Table
  (name :: Symbol)
  (columns :: [Type])
  = MkTable
  { tableName :: Builder
  }

renderTable :: Table name columns -> Builder
renderTable (MkTable{tableName}) = tableName

instance
  ( KnownSymbol name
  , IsValidColumns columns
  ) => TableInterpretable (Table name columns)
  where
  type GetTableName    (Table name _)    = name
  type GetTableColumns (Table _ columns) = GetColumnsRep columns
  type ValidateTable   (Table _ columns) = ColumnsValdationResult columns

  type TableInterpreter (Table name columns) = Table name columns
  interpretTable = MkTable{tableName = "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""}




-- ** View 

data View
  (name :: Symbol)
  (columns :: [Type])
  (parameters :: [Type])
  = MkView
    { viewName :: Builder
    , parameters :: [Builder]
    }

renderView :: View name columns parameters -> Builder
renderView (MkView{viewName, parameters}) = viewName <> renderTableParameters parameters

newtype Parameter (name :: Symbol) (chType :: Type) = MkParameter
  { renderedParameter :: Builder
  }

renderTableParameters :: [Builder] -> Builder
renderTableParameters [] = ""
renderTableParameters (x:y:z:xs) = "(" <> x <> renderTableParameters (y:z:xs) 
renderTableParameters (x:[lastParam]) = x <> lastParam <> ")"
renderTableParameters [x] = "(" <> x <> ")"


instance
  ( KnownSymbol name
  , IsValidColumns columns
  ) => TableInterpretable (View name columns '[])
  where
  type GetTableName    (View name _ _)    = name
  type GetTableColumns (View _ columns _) = GetColumnsRep columns
  type ValidateTable   (View _ columns _) = ColumnsValdationResult columns

  type TableInterpreter (View name columns '[]) = View name columns '[]
  interpretTable =
    MkView
      { viewName = "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""
      , parameters = []
      }


instance
  ( KnownSymbol name
  , IsValidColumns columns
  ) => TableInterpretable (View name columns '[Parameter name Type])
  where
  type GetTableName    (View name _ _)    = name
  type GetTableColumns (View _ columns _) = GetColumnsRep columns
  type ValidateTable   (View _ columns _) = ColumnsValdationResult columns

  type TableInterpreter (View name columns '[Parameter name Type]) = Parameter name Type -> View name columns '[]
  interpretTable MkParameter{renderedParameter} =
    MkView
      { viewName = "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""
      , parameters = [renderedParameter]
      }








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
       (     'Text "Columns description contains aplabetically unsorted columns."
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
