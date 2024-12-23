{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , LambdaCase
  , OverloadedStrings
#-}

module ClickHaskell.DbTypes
  {-# DEPRECATED "\
  \This module would be deleted in next minor release\
  \Please move its imports to ClickHaskell module\
  \" #-}
( IsChType(ToChTypeName, chTypeName, defaultValueOfTypeName)
, ToChType(toChType)
, FromChType(fromChType)
, ToQueryPart(toQueryPart)

, ChDateTime(..)
, ChDate(..)

, ChInt8(..), ChInt16(..), ChInt32(..), ChInt64(..), ChInt128(..)
, ChUInt8(..), ChUInt16(..), ChUInt32(..), ChUInt64(..), ChUInt128(..)

, ChString(..)
, ChUUID(..)

, ChArray(..)
, Nullable
, LowCardinality, IsLowCardinalitySupported

, UVarInt(..)
, module Data.WideWord
) where

-- Internal dependencies

-- External
import Data.WideWord (Int128 (..), Word128(..))

-- GHC included

import ClickHaskell
