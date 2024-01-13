{-# LANGUAGE
    AllowAmbiguousTypes
#-}

module ClickHaskell.Columns.Interpreter
( InterpretedColumn(..)
) where


-- GHC included
import Data.Text (Text)
import GHC.Base (Symbol, Type)
import GHC.TypeError (ErrorMessage)


class InterpretedColumn columnDescription where
  type GetColumnName columnDescription :: Symbol
  renderColumnName :: Text

  type GetColumnType columnDescription :: Type
  renderColumnType :: Text

  type WritableColumn    columnDescription :: Maybe ErrorMessage
  type WriteOptionalColumn columnDescription :: Maybe ErrorMessage
