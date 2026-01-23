module ClickHaskell.Primitive.TDecimal where

-- Internal
import ClickHaskell.Primitive.Serialization
import ClickHaskell.Primitive.TInt ()

-- GHC included
import Data.Binary.Get
import Data.ByteString.Builder
import Data.Fixed (Fixed (..))
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal, type(^))
import Prelude hiding (liftA2)

-- External
import Data.WideWord (Int128 (..))



-- ** Decimal32

{- |
Read the official ClickHouse documentation for the `Decimal(p, s)` type before use.

In Haskell, this type is represented as a newtype over `Fixed (10 ^ s)`,
allowing arbitrarily large integer parts, whereas ClickHouse stores decimals
as scaled `Int32` values, which may discard some of the integer part if `s` is large.

See test №6 for an example of potential truncation due to a large scale.

>>> chTypeName @(Decimal32 9 1)
"Decimal(9, 1)"
>>> 1000.1 :: Decimal32 10 1
1000.1
>>> 1000.1 :: Decimal32 10 5
1000.10000
-}
newtype Decimal32 (p :: Nat) (s :: Nat) = MkDecimal32 (Fixed (10 ^ s))

deriving newtype instance KnownNat (10^s) => Show (Decimal32 p s)
deriving newtype instance KnownNat (10^s) => Eq (Decimal32 p s)
deriving newtype instance KnownNat (10^s) => Ord (Decimal32 p s)
deriving newtype instance KnownNat (10^s) => Num (Decimal32 p s)
deriving newtype instance KnownNat (10^s) => Fractional (Decimal32 p s)

instance (KnownNat p, KnownNat s, KnownNat (10 ^ s)) => IsChType (Decimal32 p s) where
  chTypeName =
    let p = show (natVal @p Proxy)
        s = show (natVal @s Proxy)
    in "Decimal(" <> p <> ", "<> s <> ")"
  defaultValueOfTypeName = MkDecimal32 0

instance KnownNat (10 ^ s) => Serializable (Decimal32 p s) where
  serialize _ (MkDecimal32 (MkFixed int)) = int32LE $ fromIntegral int
  deserialize _ = MkDecimal32 . MkFixed . fromIntegral <$> getInt32le
  {-# INLINE deserialize #-}

instance
  ( sPowered ~ 10^s
  ) =>
  ToChType (Decimal32 p s) (Fixed sPowered) where
  toChType fixed = MkDecimal32 fixed
  fromChType (MkDecimal32 fixed) = fixed


-- ** Decimal64

{- |
Read the official ClickHouse documentation for the `Decimal(p, s)` type before use.

In Haskell, this type is represented as a newtype over `Fixed (10 ^ s)`,
allowing arbitrarily large integer parts, whereas ClickHouse stores decimals
as scaled `Int128` values, which may discard some of the integer part if `s` is large.

See test №6 for an example of potential truncation due to a large scale.

>>> chTypeName @(Decimal64 10 1)
"Decimal(10, 1)"
>>> 1000.1 :: Decimal64 10 1
1000.1
>>> 1000.1 :: Decimal64 10 5
1000.10000
-}
newtype Decimal64 (p :: Nat) (s :: Nat) = MkDecimal64 (Fixed (10 ^ s))

deriving newtype instance KnownNat (10^s) => Show (Decimal64 p s)
deriving newtype instance KnownNat (10^s) => Eq (Decimal64 p s)
deriving newtype instance KnownNat (10^s) => Ord (Decimal64 p s)
deriving newtype instance KnownNat (10^s) => Num (Decimal64 p s)
deriving newtype instance KnownNat (10^s) => Fractional (Decimal64 p s)

instance (KnownNat p, KnownNat s, KnownNat (10 ^ s)) => IsChType (Decimal64 p s) where
  chTypeName =
    let p = show (natVal @p Proxy)
        s = show (natVal @s Proxy)
    in "Decimal(" <> p <> ", "<> s <> ")"
  defaultValueOfTypeName = MkDecimal64 0

instance KnownNat (10 ^ s) => Serializable (Decimal64 p s) where
  serialize _ (MkDecimal64 (MkFixed int)) = int64LE $ fromIntegral int
  deserialize _ = MkDecimal64 . MkFixed . fromIntegral <$> getInt64le
  {-# INLINE deserialize #-}

instance
  ( sPowered ~ 10^s
  ) =>
  ToChType (Decimal64 p s) (Fixed sPowered) where
  toChType fixed = MkDecimal64 fixed
  fromChType (MkDecimal64 fixed) = fixed


-- ** Decimal128

{- |
Read the official ClickHouse documentation for the `Decimal(p, s)` type before use.

In Haskell, this type is represented as a newtype over `Fixed (10 ^ s)`,
allowing arbitrarily large integer parts, whereas ClickHouse stores decimals
as scaled `Int128` values, which may discard some of the integer part if `s` is large.

See test №6 for an example of potential truncation due to a large scale.

>>> chTypeName @(Decimal128 19 1)
"Decimal(19, 1)"
>>> 1000.1 :: Decimal128 19 1
1000.1
>>> 1000.1 :: Decimal128 19 5
1000.10000
-}
newtype Decimal128 (p :: Nat) (s :: Nat) = MkDecimal128 (Fixed (10 ^ s))

deriving newtype instance KnownNat (10^s) => Show (Decimal128 p s)
deriving newtype instance KnownNat (10^s) => Eq (Decimal128 p s)
deriving newtype instance KnownNat (10^s) => Ord (Decimal128 p s)
deriving newtype instance KnownNat (10^s) => Num (Decimal128 p s)
deriving newtype instance KnownNat (10^s) => Fractional (Decimal128 p s)

instance (KnownNat p, KnownNat s, KnownNat (10 ^ s)) => IsChType (Decimal128 p s) where
  chTypeName =
    let p = show (natVal @p Proxy)
        s = show (natVal @s Proxy)
    in "Decimal(" <> p <> ", "<> s <> ")"
  defaultValueOfTypeName = 0

instance Serializable (Decimal128 p s) where
  serialize rev (MkDecimal128 (MkFixed int)) = serialize @Int128 rev (fromIntegral int)
  deserialize rev = MkDecimal128 . MkFixed . fromIntegral <$> deserialize @Int128 rev
  {-# INLINE deserialize #-}

instance
  ( sPowered ~ 10^s
  ) =>
  ToChType (Decimal128 p s) (Fixed sPowered) where
  toChType fixed = MkDecimal128 fixed
  fromChType (MkDecimal128 fixed) = fixed
