module ClickHaskell.Native.Serialization where

-- Internal dependencies
import ClickHaskell.DbTypes

-- GHC included
import Data.ByteString as BS (length)
import Data.ByteString.Builder as BS (Builder, byteString, int16LE, int32LE, int64LE, int8, word64LE, word8, word32LE, word16LE)
import Data.ByteString.Char8 as BS8 (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Bits (Bits (..))
import Data.WideWord (Int128(..), Word128 (..))
import Foreign (Ptr)

{- |
  Unsigned variable-length quantity encoding
-}
uVarInt :: (Bits a, Num a, Integral a) => a -> Builder
uVarInt = go
  where
  go i
    | i < 0x80 = word8 (fromIntegral i)
    | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)

{-# SPECIALIZE uVarInt :: ChUInt8 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt16 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt32 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt64 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt128 -> Builder #-}




class Serializable chType
  where
  serialize :: chType -> Builder

instance Serializable ChUInt8 where
  serialize = word8 . fromChType @ChUInt8 @Word8

instance Serializable ChUInt16 where
  serialize = word16LE . fromChType @ChUInt16 @Word16

instance Serializable ChUInt32 where
  serialize = word32LE . fromChType @ChUInt32 @Word32

instance Serializable ChUInt64 where
  serialize = word64LE . fromChType @ChUInt64 @Word64

instance Serializable ChUInt128 where
  serialize = (\word128 -> word64LE (word128Hi64 word128) <> word64LE (word128Lo64 word128))
    . fromChType @ChUInt128 @Word128

instance Serializable ChInt8 where
  serialize = int8 . fromChType @ChInt8 @Int8

instance Serializable ChInt16 where
  serialize = int16LE . fromChType @ChInt16 @Int16

instance Serializable ChInt32 where
  serialize = int32LE . fromChType @ChInt32 @Int32

instance Serializable ChInt64 where
  serialize = int64LE . fromChType @ChInt64 @Int64

instance Serializable ChInt128 where
  serialize
    = (\int128 -> word64LE (int128Hi64 int128) <> word64LE (int128Lo64 int128))
    . fromChType @ChInt128 @Int128

instance Serializable ChString where
  serialize str
    =  (uVarInt @ChUInt64 . fromIntegral . BS.length . fromChType @_ @ByteString) str
    <> (BS.byteString . fromChType @_ @ByteString) str




class
  Deserializable chType
  where
  deserialize :: Ptr Word8 -> chType
