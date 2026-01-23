module ClickHaskell.Primitive.TEnum where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Data.Bits (Bits)
import Data.ByteString.Builder (byteString, int8, int16LE)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Int (Int8, Int16)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- External
import Data.Binary.Get (getInt8, getInt16le)


-- * Enum8

newtype Enum8 (enums :: Symbol) = MkEnum8 Int8
  deriving newtype (Show, Eq, Num, Bits, Bounded, Enum)

instance KnownSymbol enums => IsChType (Enum8 enums) where
  chTypeName = "Enum8(" <> symbolVal @enums Proxy <> ")"
  defaultValueOfTypeName = 0

instance Serializable (Enum8 enums)  where
  serialize _ (MkEnum8 i8) = int8 i8
  deserialize _ = MkEnum8 <$> getInt8
  {-# INLINE deserialize #-}

instance ToChType (Enum8 enums) Int8 where
  toChType = MkEnum8
  fromChType (MkEnum8 i8)= i8

instance ToQueryPart (Enum8 enums) where
  toQueryPart = byteString . BS8.pack . show . fromChType @_ @Int8


-- * Enum16

newtype Enum16 (enums :: Symbol) = MkEnum16 Int16
  deriving newtype (Show, Eq, Num, Bits, Bounded, Enum)

instance KnownSymbol enums => IsChType (Enum16 enums) where
  chTypeName = "Enum16(" <> symbolVal @enums Proxy <> ")"
  defaultValueOfTypeName = 0

instance Serializable (Enum16 enums)  where
  serialize _ (MkEnum16 i16) = int16LE i16
  deserialize _ = MkEnum16 <$> getInt16le
  {-# INLINE deserialize #-}

instance ToChType (Enum16 enums) Int16 where
  toChType = MkEnum16
  fromChType (MkEnum16 i16) = i16

instance ToQueryPart (Enum16 enums) where
  toQueryPart = byteString . BS8.pack . show . fromChType @_ @Int16


