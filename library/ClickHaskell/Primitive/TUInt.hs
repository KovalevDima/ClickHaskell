{-# OPTIONS_GHC -Wno-orphans #-}
module ClickHaskell.Primitive.TUInt where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Data.ByteString.Builder (byteString, word8, word16LE, word32LE, word64LE)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Word (Word8, Word16, Word32, Word64)

-- External
import Data.Binary.Get (getWord8, getWord16le, getWord32le, getWord64le)
import Data.WideWord


-- * UInt8

{- | ClickHouse UInt8 column type -}
type UInt8 = Word8

instance IsChType UInt8 where
  chTypeName = "UInt8"
  defaultValueOfTypeName = 0

instance Serializable UInt8 where
  serialize _ = word8
  deserialize _ = getWord8
  {-# INLINE deserialize #-}

instance ToQueryPart UInt8 where
  toQueryPart = byteString . BS8.pack . show


-- * UInt16


{- | ClickHouse UInt16 column type -}
type UInt16 = Word16

instance IsChType UInt16 where
  chTypeName = "UInt16"
  defaultValueOfTypeName = 0

instance Serializable UInt16 where
  serialize _ = word16LE
  deserialize _ = getWord16le
  {-# INLINE deserialize #-}

instance ToQueryPart UInt16 where
  toQueryPart = byteString . BS8.pack . show


-- * UInt32

{- | ClickHouse UInt32 column type -}
type UInt32 = Word32

instance IsChType UInt32 where
  chTypeName = "UInt32"
  defaultValueOfTypeName = 0

instance Serializable UInt32 where
  serialize _ = word32LE
  deserialize _ = getWord32le
  {-# INLINE deserialize #-}

instance ToQueryPart UInt32 where
  toQueryPart = byteString . BS8.pack . show


-- * UInt64

{- | ClickHouse UInt64 column type -}
type UInt64 = Word64

instance IsChType UInt64 where
  chTypeName = "UInt64"
  defaultValueOfTypeName = 0

instance Serializable UInt64 where
  serialize _ = word64LE
  deserialize _ = getWord64le
  {-# INLINE deserialize #-}

instance ToQueryPart UInt64 where
  toQueryPart = byteString . BS8.pack . show


-- * UInt128

{- | ClickHouse UInt128 column type -}
type UInt128 = Word128

instance IsChType UInt128 where
  chTypeName = "UInt128"
  defaultValueOfTypeName = 0

instance Serializable UInt128 where
  serialize _ = (\(Word128 hi lo) -> word64LE lo <> word64LE hi)
  deserialize _ = do
    low <- getWord64le
    high <- getWord64le
    pure $ Word128 high low
  {-# INLINE deserialize #-}

instance ToQueryPart UInt128 where
  toQueryPart w128 = "'" <> (byteString . BS8.pack . show) w128 <> "'"


-- * UInt256

{- | ClickHouse UInt128 column type -}
type UInt256 = Word256

instance IsChType UInt256 where
  chTypeName = "UInt256"
  defaultValueOfTypeName = 0

instance Serializable UInt256 where
  serialize _ = (\(Word256 high mid1 mid0 low) -> word64LE low <> word64LE mid0 <> word64LE mid1 <> word64LE high)
  deserialize _ = do
    low <- getWord64le
    mid0 <- getWord64le
    mid1 <- getWord64le
    high <- getWord64le
    pure $ Word256 high mid1 mid0 low
  {-# INLINE deserialize #-}

instance ToQueryPart UInt256 where
  toQueryPart w256 = "'" <> (byteString . BS8.pack . show) w256 <> "'"