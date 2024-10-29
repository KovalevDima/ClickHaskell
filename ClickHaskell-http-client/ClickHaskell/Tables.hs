{-# LANGUAGE
    DataKinds
  , PolyKinds
#-}

module ClickHaskell.Tables
( module ClickHaskell.NativeProtocol.Columns
, module ClickHaskell.NativeProtocol.Parameters
, Table, View
) where


-- Internal
import ClickHaskell.NativeProtocol.Columns (CompiledColumn(..), Default, Alias, Column(..), TakeColumn, Columns(..), columnsCount, rowsCount, appendColumn, emptyColumns)
import ClickHaskell.NativeProtocol.Parameters (CheckParameters, InterpretableParameters(..), ParametersInterpreter(..), Parameter, parameters, parameter)
import ClickHaskell (View, Table)
