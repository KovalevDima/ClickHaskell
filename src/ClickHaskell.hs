{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , UndecidableInstances
  #-}

{-# OPTIONS_GHC
  -Wno-unrecognised-pragmas
#-}

module ClickHaskell
  (
  -- * Tables description DSL
    module ClickHaskell.TableDsl

  -- * Data manipulation DSL 
  , module ClickHaskell.DataDsl

  -- * Databases types
  , module ClickHaskell.DbTypes

  -- * Simple buffering abstractions
  , module ClickHaskell.Buffering

  -- * Client abstraction
  , module ClickHaskell.Client
  ) where

-- Internal dependencies
import ClickHaskell.Buffering
import ClickHaskell.Client
import ClickHaskell.DataDsl
import ClickHaskell.DbTypes
import ClickHaskell.TableDsl
