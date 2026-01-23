{-# OPTIONS_GHC -Wno-orphans #-}
module ClickHaskell.Primitive.TFloat where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Data.Binary.Get
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS8 (pack)
import Prelude hiding (liftA2)

-- External
import Data.Binary.Put (putFloatle, putDoublele, execPut)


-- * Float32

type Float32 = Float

instance IsChType Float32 where
  chTypeName = "Float32"
  defaultValueOfTypeName = 0

instance Serializable Float32 where
  serialize _ f32 = (execPut . putFloatle) f32
  deserialize _ = getFloatle
  {-# INLINE deserialize #-}

instance ToQueryPart Float32 where
  toQueryPart = byteString . BS8.pack . show



-- * Float64

type Float64 = Double

instance IsChType Float64 where
  chTypeName = "Float64"
  defaultValueOfTypeName = 0

instance Serializable Float64 where
  serialize _ f64 = (execPut . putDoublele) f64
  deserialize _ = getDoublele
  {-# INLINE deserialize #-}

instance ToQueryPart Float64 where
  toQueryPart = byteString . BS8.pack . show