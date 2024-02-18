{-# LANGUAGE
    DataKinds
#-}

{-# OPTIONS_GHC
  -Wno-orphans
  -Wno-missing-methods
#-}

module ClickHaskell.Tables.Errors
() where


-- Internal
import ClickHaskell.Tables.Exports
  ( InterpretableTable(..)
  , CompiledColumns(..)
  , CompiledColumn(..)
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
    (    'Text "Expected a column description. But got:"
    :$$: ShowType unsupportedColumnDescription
    )
  ) => CompiledColumn unsupportedColumnDescription
