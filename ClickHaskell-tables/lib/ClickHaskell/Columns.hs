{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , InstanceSigs
  , OverloadedStrings
  , NamedFieldPuns
  , UndecidableInstances
#-}

module ClickHaskell.Columns
( CompiledColumn(..)
, Column
, AliasColumn
, DefaultableColumn
) where


-- Internal
import ClickHaskell.Columns.Alias (AliasColumn)
import ClickHaskell.Columns.Column (Column)
import ClickHaskell.Columns.Default (DefaultableColumn)
import ClickHaskell.Columns.Compiler (CompiledColumn(..))
