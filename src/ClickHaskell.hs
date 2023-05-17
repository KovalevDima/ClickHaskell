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
  , module ClickHaskell.TableDsl.DbTypes

  -- * Data manipulation DSL 
  , module ClickHaskell.DataDsl

  -- * Buffering abstractions
  , module ClickHaskell.Buffering

  -- * Client abstraction
  , module ClickHaskell.Client

  -- Reexports
  , Proxy(..), someSymbolVal, SomeSymbol(..), Generic, Word32, Word64,
  ) where

-- Internal dependencies
import ClickHaskell.Buffering
import ClickHaskell.Client
import ClickHaskell.DataDsl
import ClickHaskell.TableDsl
import ClickHaskell.TableDsl.DbTypes

-- GHC included libraries imports
import Data.Data                  (Proxy (..))
import Data.Word                  (Word64, Word32)
import GHC.Generics               (Generic)
import GHC.TypeLits               (someSymbolVal, SomeSymbol (SomeSymbol))
