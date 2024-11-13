{-# LANGUAGE
    DataKinds
  , PolyKinds
#-}

module ClickHaskell.Tables
( module ClickHaskell.Columns
, module ClickHaskell.Parameters
, Table, View
, KnownColumn(..), Column(..), Columns(..), 
) where


-- Internal
import ClickHaskell.DbTypes
import ClickHaskell.Columns (TakeColumn, columnsCount, rowsCount, appendColumn, emptyColumns)
import ClickHaskell.Parameters (CheckParameters, InterpretableParameters(..), ParametersInterpreter, Parameters(..), Parameter, parameters, parameter)
import ClickHaskell (View, Table)
