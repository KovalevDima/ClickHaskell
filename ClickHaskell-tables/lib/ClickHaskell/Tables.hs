{-# LANGUAGE
    DataKinds
#-}

{-# OPTIONS_GHC
  -Wno-orphans
  -Wno-missing-methods
#-}

module ClickHaskell.Tables
(
-- * Tables
-- ** Interpreter
  InterpretableTable(..)

-- ** Table
, Table, renderTable

-- ** View
, View, renderView
, Parameter, mkParameter


-- * Columns
-- ** Compiler
, CompiledColumn(..)
, CompiledColumns(..)

-- ** Column
, Column

-- ** Alias column 
, AliasColumn
) where


-- Internal
import ClickHaskell.Columns
  ( CompiledColumn(..)
  , AliasColumn
  , Column
  )
import ClickHaskell.Tables.Interpreter
  ( InterpretableTable(..)
  , CompiledColumns(..)
  )
import ClickHaskell.Tables.Table (Table, renderTable)
import ClickHaskell.Tables.View
  ( View, renderView
  , Parameter, mkParameter
  )


-- GHC included
import GHC.TypeLits (TypeError, ErrorMessage (..))


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a valid table description. But got:"
    :$$: ShowType something
    )
  ) => InterpretableTable something

instance
  ( TypeError
    (    'Text "Data source should have at least one column"
    :$$: 'Text "But given source without columns"
    )
  ) => CompiledColumns '[]

instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a column description. But got: "
    :$$: ShowType unsupportedColumnDescription
    )
  ) => CompiledColumn unsupportedColumnDescription
