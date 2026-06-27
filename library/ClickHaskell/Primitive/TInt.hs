{-# OPTIONS_GHC -Wno-orphans #-}
module ClickHaskell.Primitive.TInt
  ( Int8, Int16, Int32, Int64, Int128, Int256
  ) where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Data.Int (Int8, Int16, Int32, Int64)
import Data.ByteString.Builder (byteString, int8, int16LE, int32LE, int64LE, word64LE)
import Data.ByteString.Char8 as BS8 (pack)

-- External
import Data.Binary.Get (getInt8, getInt16le, getInt32le, getInt64le, getWord64le)
import Data.WideWord


instance IsChType Int8 where
  chTypeName = "Int8"
  defaultValueOfTypeName = 0

instance Serializable Int8 where
  serialize _ = int8
  deserialize _ = getInt8
  {-# INLINE deserialize #-}

instance ToQueryPart Int8 where
  toQueryPart = byteString . BS8.pack . show


-- * Int16

instance IsChType Int16 where
  chTypeName = "Int16"
  defaultValueOfTypeName = 0

instance Serializable Int16 where
  serialize _ = int16LE
  deserialize _ = getInt16le
  {-# INLINE deserialize #-}

instance ToQueryPart Int16 where
  toQueryPart = byteString . BS8.pack . show


-- * Int32

instance IsChType Int32 where
  chTypeName = "Int32"
  defaultValueOfTypeName = 0

instance Serializable Int32 where
  serialize _ = int32LE
  deserialize _ = getInt32le
  {-# INLINE deserialize #-}

instance ToQueryPart Int32 where
  toQueryPart = byteString . BS8.pack . show







instance IsChType Int64 where
  chTypeName = "Int64"
  defaultValueOfTypeName = 0

instance Serializable Int64 where
  serialize _ = int64LE
  deserialize _ = getInt64le
  {-# INLINE deserialize #-}

instance ToQueryPart Int64 where
  toQueryPart = byteString . BS8.pack . show








instance IsChType Int128 where
  chTypeName = "Int128"
  defaultValueOfTypeName = 0

instance Serializable Int128 where
  serialize _ = (\(Int128 hi lo) -> word64LE lo <> word64LE hi)
  deserialize _ = do
    low <- getWord64le
    high <- getWord64le
    pure $ Int128 high low
  {-# INLINE deserialize #-}

instance ToQueryPart Int128 where
  toQueryPart x = "'" <> (byteString . BS8.pack . show) x <> "'"









instance IsChType Int256 where
  chTypeName = "Int256"
  defaultValueOfTypeName = 0

instance Serializable Int256 where
  serialize _ = (\(Int256 a1 a2 a3 a4) -> word64LE a4 <> word64LE a3 <> word64LE a2 <> word64LE a1)
  deserialize _ = do
    low <- getWord64le
    mid1 <- getWord64le
    mid2 <- getWord64le
    high <- getWord64le
    pure $ Int256 high mid2 mid1 low
  {-# INLINE deserialize #-}

instance ToQueryPart Int256 where
  toQueryPart x = "'" <> (byteString . BS8.pack . show) x <> "'"

