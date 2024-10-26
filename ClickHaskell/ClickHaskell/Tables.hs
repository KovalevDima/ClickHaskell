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
import ClickHaskell.DbTypes (IsChType(..), UVarInt)


-- GHC included
import Data.ByteString.Builder as BS (Builder, stringUtf8)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type)
import GHC.TypeLits            (TypeError, ErrorMessage (..), Symbol, KnownSymbol, symbolVal)
import Data.Type.Bool          (If)
import Data.Type.Equality      (type(==))

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
