{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , UndecidableInstances
#-}

module ClickHaskell.Columns.Compiler
( CompiledColumn(..)
) where


-- GHC included
import Data.Text (Text)
import GHC.Base (Symbol, Type)
import GHC.TypeError (ErrorMessage(..))


class CompiledColumn columnDescription where
  type GetColumnName columnDescription :: Symbol
  renderColumnName :: Text

  type GetColumnType columnDescription :: Type
  renderColumnType :: Text

  type WritableColumn    columnDescription :: Maybe ErrorMessage
  type WriteOptionalColumn columnDescription :: Maybe ErrorMessage
