{-# OPTIONS_GHC -Wno-orphans #-}
module ClickHaskell.Primitive.TBool where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Data.ByteString.Builder (int8)
import Data.Bool ( bool )

-- External
import Data.Binary.Get (getInt8)


instance IsChType Bool where
  chTypeName = "Bool"
  defaultValueOfTypeName = False

instance Serializable Bool where
  serialize _ = int8 . bool 0 1
  deserialize _ = (\int -> case int of 0->False; _->True) <$> getInt8
  {-# INLINE deserialize #-}

instance ToQueryPart Bool where
  toQueryPart = bool "false" "true"
