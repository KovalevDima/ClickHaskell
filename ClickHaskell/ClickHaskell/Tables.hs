{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
  , TypeFamilyDependencies
  , UndecidableInstances
  , GADTs
  , ScopedTypeVariables
#-}

module ClickHaskell.Tables
(
-- * Specs
  Table
, View

, parameter
, Parameter

, parameters
, ParametersInterpreter(..)

, InterpretableParameters(..)
, CheckParameters

-- * Columns
, Columns(..)
, columnsCount
, rowsCount

, appendColumn
, emptyColumns

-- ** HasColumns helper class
, HasColumns(..)

-- ** Take column from list of columns
, TakeColumn

-- ** Column
, mkColumn
, Column(..)

-- ** Column subtypes
, Alias
, Default

-- ** Compilers
, CompiledColumn(..)
) where


-- Internal
import ClickHaskell.NativeProtocol.Columns
import ClickHaskell.NativeProtocol.Parameters (CheckParameters, InterpretableParameters(..), ParametersInterpreter(..), Parameter, parameters, parameter)

-- GHC included
import Data.Kind               (Type)
import GHC.TypeLits            (Symbol)

-- * Specs

data Table
  (name :: Symbol)
  (columns :: [Type])

data View
  (name :: Symbol)
  (columns :: [Type])
  (parameters :: [Type])


-- ** HasColumns helper class

class HasColumns (hasColumns :: k) where
  type GetColumns hasColumns :: [Type]

instance HasColumns (View name columns params) where
  type GetColumns (View _ columns _) = columns

instance HasColumns (Table name columns) where
  type GetColumns (Table _ columns) = columns

instance HasColumns (columns :: [Type]) where
  type GetColumns columns = columns
