{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , OverloadedStrings
  , TypeFamilyDependencies
  , UndecidableInstances
#-}

module ClickHaskell.Tables.Interpreter
( InterpretableTable(..)
, CompiledColumns(..)
) where

-- Internal
import ClickHaskell.Columns.Compiler
  ( CompiledColumn(..)
  )


-- GHC included
import Data.Text          (Text)
import Data.Kind          (Type)
import Data.Type.Bool     (If)
import Data.Type.Equality (type(==))
import Data.Type.Ord      (type(>?))
import GHC.TypeLits       (ErrorMessage (..), Symbol, TypeError)


class
  InterpretableTable table
  where
  type GetTableName table :: Symbol
  type GetTableColumns table :: [Type]
  type ValidatedTable table :: Maybe ErrorMessage

  type TableInterpreter table = result | result -> table
  interpretTable :: TableInterpreter table




class CompiledColumns (columns :: [Type])
  where
  type GetColumnsRep columns :: [Type]
  getRenederedColumns :: [(Text, Text)]
  
  type ColumnsCompilationResult columns :: Maybe ErrorMessage




instance
  ( CompiledColumn column1
  , CompiledColumn column2
  , CompiledColumns (column2 ': columns)
  ) => CompiledColumns (column1 ': column2 ': columns)
  where
  type GetColumnsRep (column1 ': column2 ': columns) = column1 ': GetColumnsRep (column2 ': columns)
  getRenederedColumns
    = (renderColumnName @column1, renderColumnType @column1)
    : getRenederedColumns @(column2 ': columns)

  type (ColumnsCompilationResult (column1 ': column2 ': columns)) =
    If (GetColumnName column1 >? GetColumnName column2)
      ( TypeError
        (    'Text "Columns description contains aplabetically unsorted columns"
        :$$: 'Text "Column with name \""         :<>: 'Text (GetColumnName column1) :<>: 'Text "\" "
          :<>: 'Text "should be placed after \"" :<>: 'Text (GetColumnName column2) :<>: 'Text "\""
        )
      )
      (If (GetColumnName column2 == GetColumnName column1)
        (TypeError
          (    'Text "There are two columns with identical name: \""
          :<>: 'Text (GetColumnName column1) :<>: 'Text "\"")
          )
        (ColumnsCompilationResult (column2 ': columns))
      )


instance
  ( CompiledColumn column
  ) => CompiledColumns '[column]
  where
  type GetColumnsRep '[column] = '[column]
  getRenederedColumns = [(renderColumnName @column, renderColumnType @column)]

  type (ColumnsCompilationResult '[column]) = 'Nothing
