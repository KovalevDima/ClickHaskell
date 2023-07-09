module ClickHaskell.DataDsl
  ( module ClickHaskell.DataDsl.Inserting
  , module ClickHaskell.DataDsl.Selecting
  
  -- reexports
  , Proxy(..), someSymbolVal, SomeSymbol(..), Generic
  ) where

-- Internal dependencies
import ClickHaskell.DataDsl.Inserting
import ClickHaskell.DataDsl.Selecting

-- GHC included libraries imports
import Data.Data    (Proxy(..))
import GHC.Generics (Generic)
import GHC.TypeLits (someSymbolVal, SomeSymbol(..))
