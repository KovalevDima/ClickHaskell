module ClickHaskell.Parameters
  {-# DEPRECATED "\
  \This module would be deleted in next minor release\
  \Please move its imports to ClickHaskell module\
  \" #-}
  ( Parameter
  , parameters
  , parameter
  , renderParameters
  , Parameters(..)
  , renderParameter
  , CheckParameters
  ) where

import ClickHaskell.NativeProtocol
import Data.Binary.Builder (Builder)

{-# DEPRECATED parameters "\
  \This function would be deleted in next minor release\
  \Please use viewParameters instead"
#-}
parameters :: (Parameters '[] -> Parameters passedParameters) -> Builder
parameters = viewParameters
