{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , OverloadedStrings
  , UndecidableInstances
#-}

module ClickHaskell.Tables.Interpreter
( TableInterpretable(..)

, IsValidColumns(..)
) where


-- Internal
import ClickHaskell.Columns
  ( InterpretedColumn
    ( GetColumnName
    , renderColumnName
    , renderColumnType
    )
  )


-- GHC included
import Data.Text          (Text)
import Data.Kind          (Type)
import Data.Type.Bool     (If)
import Data.Type.Equality (type(==))
import Data.Type.Ord      (type(>?))
import GHC.TypeLits       (ErrorMessage (..), Symbol, TypeError)


class
  TableInterpretable table
  where
  type GetTableName table :: Symbol
  type GetTableColumns table :: [Type]
  type ValidatedTable table :: Maybe ErrorMessage

  type TableInterpreter table :: Type
  interpretTable :: TableInterpreter table




class IsValidColumns (columns :: [Type])
  where
  type GetColumnsRep columns :: [Type]
  getRenederedColumns :: [(Text, Text)]
  
  type ColumnsValdationResult columns :: Maybe ErrorMessage




instance
  ( InterpretedColumn column1
  , InterpretedColumn column2
  , IsValidColumns (column2 ': columns)
  ) => IsValidColumns (column1 ': column2 ': columns)
  where
  type GetColumnsRep (column1 ': column2 ': columns) = column1 ': GetColumnsRep (column2 ': columns)
  getRenederedColumns = (renderColumnName @column1, renderColumnType @column2) : getRenederedColumns @(column2 ': columns)

  type (ColumnsValdationResult (column1 ': column2 ': columns)) =
    If (GetColumnName column1 >? GetColumnName column2)
      ( TypeError
        (    'Text "Columns description contains aplabetically unsorted columns"
        :$$: 'Text "Column with name \""         :<>: 'Text (GetColumnName column1) :<>: 'Text "\" "
          :<>: 'Text "should be placed after \"" :<>: 'Text (GetColumnName column2) :<>: 'Text "\""
        )
      )
      (If (GetColumnName column2 == GetColumnName column1)
        (TypeError ('Text "There are two columns with identical name: \"" :<>: 'Text (GetColumnName column1) :<>: 'Text "\""))
        (ColumnsValdationResult (column2 ': columns))
      )


instance
  ( InterpretedColumn column
  ) => IsValidColumns '[column]
  where
  type GetColumnsRep '[column] = '[column]
  getRenederedColumns = [(renderColumnName @column, renderColumnType @column)]

  type (ColumnsValdationResult '[column]) = 'Nothing
