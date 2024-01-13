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
  TableInterpretable(..)
, IsValidColumns(..)

-- ** Table
, Table, renderTable

-- ** View
, View, renderView
, Parameter, mkParameter


-- * Columns
-- ** Intepreter
, InterpretedColumn(..)

-- ** Column
, Column

-- ** Alias column 
, AliasColumn
) where


-- Internal
import ClickHaskell.Columns
  ( InterpretedColumn(..)
  , Column
  , AliasColumn
  )
import ClickHaskell.Tables.Interpreter
  ( TableInterpretable(..)
  , IsValidColumns(..)
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
  ) => TableInterpretable something

instance
  ( TypeError
    (    'Text "Data source should have at least one column"
    :$$: 'Text "But given source without columns"
    )
  ) => IsValidColumns '[]

instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected a column description. But got: "
    :$$: ShowType unsupportedColumnDescription
    )
  ) => InterpretedColumn unsupportedColumnDescription
