{-# LANGUAGE
    DataKinds
  , PolyKinds
#-}

module ClickHaskell.Tables
( module ClickHaskell.Columns
, module ClickHaskell.Parameters
, Table, View
) where


-- Internal
import ClickHaskell.Columns (KnownColumn(..), Column(..), TakeColumn, Columns(..), columnsCount, rowsCount, appendColumn, emptyColumns)
import ClickHaskell.Parameters (CheckParameters, InterpretableParameters(..), ParametersInterpreter, Parameters(..), Parameter, parameters, parameter)
import ClickHaskell (View, Table)
