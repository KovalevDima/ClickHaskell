module ClickHaskell.Primitive.TDateTime where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Control.DeepSeq (NFData)
import Data.Bits (Bits)
import Data.ByteString.Builder (byteString, word32LE, word64LE)
import Data.ByteString.Char8 as BS8 (length, pack, replicate)
import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word64)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

-- External
import Data.Binary.Get (getWord32le, getWord64le)

-- * DateTime

{- |
ClickHouse DateTime column type (parametrized with timezone)

>>> chTypeName @(DateTime "")
"DateTime"
>>> chTypeName @(DateTime "UTC")
"DateTime('UTC')"

__Note:__ 'DateTime' stores whole seconds only, so converting from 'UTCTime' \
will drop any sub-second precision.

>>> let myUtcTime = posixSecondsToUTCTime 0.042_042
>>> toChType @(DateTime "") @UTCTime myUtcTime
0
-}
newtype DateTime (tz :: Symbol) = MkDateTime Word32
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance KnownSymbol tz => IsChType (DateTime tz)
  where
  chTypeName = case symbolVal @tz Proxy of
    "" -> "DateTime"
    tz -> "DateTime('" <> tz <> "')"
  defaultValueOfTypeName = MkDateTime 0

instance Serializable (DateTime tz) where
  serialize _ (MkDateTime w32) = word32LE w32
  deserialize _ = MkDateTime <$> getWord32le
  {-# INLINE deserialize #-}

instance ToChType (DateTime tz) Word32     where
  toChType = MkDateTime
  fromChType (MkDateTime w32)= w32

instance ToChType (DateTime tz) UTCTime    where
  toChType = MkDateTime . floor . utcTimeToPOSIXSeconds
  fromChType (MkDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)

instance ToQueryPart (DateTime tz)
  where
  toQueryPart chDateTime = let time = BS8.pack . show . coerce @(DateTime tz) @Word32 $ chDateTime
    in byteString (BS8.replicate (10 - BS8.length time) '0' <> time)


-- * DateTime64

{- |
ClickHouse DateTime64 column type (parametrized with timezone)

>>> chTypeName @(DateTime64 3 "")
"DateTime64(3)"
>>> chTypeName @(DateTime64 3 "UTC")
"DateTime64(3, 'UTC')"

__Note:__ conversion from 'UTCTime' may lose sub-second precision if \
the @precision@ parameter is lower than the actual timestamp precision.

>>> let myUtcTime = posixSecondsToUTCTime 42.000_000_042
>>> toChType @(DateTime64 6 "") @UTCTime myUtcTime
42000000
>>> toChType @(DateTime64 9 "") @UTCTime myUtcTime
42000000042
-}
newtype DateTime64 (precision :: Nat) (tz :: Symbol) = MkDateTime64 Word64
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance
  (KnownSymbol tz, KnownNat precision)
  =>
  IsChType (DateTime64 precision tz)
  where
  chTypeName =
    let
      prec = show (natVal @precision Proxy)
    in
    case symbolVal @tz Proxy of
      "" -> "DateTime64(" <> prec <> ")"
      tz -> "DateTime64(" <> prec <> ", '" <> tz <> "')"
  defaultValueOfTypeName = MkDateTime64 0

instance Serializable (DateTime64 precision tz) where
  serialize _ (MkDateTime64 w64) = word64LE w64
  deserialize _ = MkDateTime64 <$> getWord64le
  {-# INLINE deserialize #-}

instance ToChType (DateTime64 precision tz) Word64 where
  toChType = MkDateTime64
  fromChType (MkDateTime64 w64) = w64

instance KnownNat precision => ToChType (DateTime64 precision tz) UTCTime where
  toChType = MkDateTime64 . floor . (* (10 ^ natVal (Proxy @precision)))
    . utcTimeToPOSIXSeconds
  fromChType (MkDateTime64 w64) = posixSecondsToUTCTime
    $ (/ (10 ^ natVal (Proxy @precision))) $ fromIntegral w64

-- ToDo: Need to be fixed
-- instance ToQueryPart (DateTime64 precision tz)
--   where
--   toQueryPart chDateTime =
--     let time = BS8.pack . show . fromChType @_ @Word64 $ chDateTime
--     in byteString (BS8.replicate (12 - BS8.length time) '0' <> time)
