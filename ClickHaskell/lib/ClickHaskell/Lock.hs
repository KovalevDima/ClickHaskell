{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DuplicateRecordFields
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}

module ClickHaskell.Lock
  ( LockPart(..)
  , LockedColumn(..)
  , LockedParameter(..)
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)

data LockPart =
  Table
    { name :: Text
    , columns :: [LockedColumn]
    }
  |
  View
    { name :: Text
    , columns :: [LockedColumn]
    , parameters :: [LockedParameter]
    }
  deriving (Generic)




data LockedColumn = MkLockedColumn
  { name :: Text
  , columnType :: Text
  }
  deriving (Generic)




data LockedParameter = MkLockedParameter
  { name :: Text
  , parameterType :: Text
  }
  deriving (Generic)
