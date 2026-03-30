module ClickHaskell.Primitive.TDecimal where

-- Internal
import ClickHaskell.Primitive.Serialization
import ClickHaskell.Primitive.TInt (Int64, Int32)

-- GHC included
import Data.Binary.Get
import Data.ByteString.Builder
import Data.Fixed (Fixed (..))
import Data.Kind (Constraint)
import Data.Type.Bool
import Data.Type.Ord
import Data.Typeable (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, TypeError, natVal, type (^))

-- External
import Data.WideWord (Int128 (..), Int256)
import Data.ByteString.Char8 as BS8 (pack)


-- ** Decimal32

{- |
>>> chTypeName @(Decimal32 9 1)
"Decimal(9, 1)"
>>> toChType @(Decimal32 1 1) @(Fixed (10^1)) 100000.1
100000.1
>>> toChType @(Decimal32 9 5) @(Fixed (10^5)) 10000.1
10000.10000
>>> toChType @(Decimal32 9 5) @(Fixed (10^5)) 100000.1
14100.75408
-}
newtype Decimal32 (p :: Nat) (s :: Nat) = MkDecimal32 Int32

instance KnownNat (10^s) => Show (Decimal32 p s) where
  show (MkDecimal32 int32) = show $ MkFixed @_ @(10^s)(fromIntegral int32)
deriving newtype instance KnownNat (10^s) => Eq (Decimal32 p s)
deriving newtype instance KnownNat (10^s) => Ord (Decimal32 p s)
deriving newtype instance KnownNat (10^s) => Num (Decimal32 p s)

instance
  (ValidRanges 32 1 9 p s, KnownNat p, KnownNat s, KnownNat (10 ^ s))
  =>
  IsChType (Decimal32 p s) where
  chTypeName =
    let p = show (natVal @p Proxy)
        s = show (natVal @s Proxy)
    in "Decimal(" <> p <> ", "<> s <> ")"
  defaultValueOfTypeName = MkDecimal32 0

instance KnownNat (10 ^ s) => Serializable (Decimal32 p s) where
  serialize _ (MkDecimal32 int) = int32LE int
  deserialize _ = MkDecimal32 <$> getInt32le
  {-# INLINE deserialize #-}

instance
  ( sPowered ~ 10^s
  , IsChType (Decimal32 p s)
  ) =>
  ToChType (Decimal32 p s) (Fixed sPowered) where
  toChType (MkFixed fixedRep) = MkDecimal32 (fromIntegral fixedRep)
  fromChType (MkDecimal32 int32) = MkFixed (fromIntegral int32)

instance KnownNat (10^s) => ToQueryPart (Decimal32 p s) where
  toQueryPart dec = byteString (BS8.pack $ show dec)


-- ** Decimal64

{- |
>>> chTypeName @(Decimal64 10 1)
"Decimal(10, 1)"
>>> toChType @(Decimal64 10 1) @(Fixed (10^1)) 1000.1
1000.1
>>> toChType @(Decimal64 10 5) @(Fixed (10^5)) 1000.1
1000.10000
-}
newtype Decimal64 (p :: Nat) (s :: Nat) = MkDecimal64 Int64

instance KnownNat (10^s) => Show (Decimal64 p s) where
  show (MkDecimal64 int64) = show $ MkFixed @_ @(10^s)(fromIntegral int64)
deriving newtype instance KnownNat (10^s) => Eq (Decimal64 p s)
deriving newtype instance KnownNat (10^s) => Ord (Decimal64 p s)

instance
  (ValidRanges 64 10 18 p s, KnownNat p, KnownNat s, KnownNat (10 ^ s))
  =>
  IsChType (Decimal64 p s) where
  chTypeName =
    let p = show (natVal @p Proxy)
        s = show (natVal @s Proxy)
    in "Decimal(" <> p <> ", "<> s <> ")"
  defaultValueOfTypeName = MkDecimal64 0

instance KnownNat (10 ^ s) => Serializable (Decimal64 p s) where
  serialize _ (MkDecimal64 int) = int64LE int
  deserialize _ = MkDecimal64 <$> getInt64le
  {-# INLINE deserialize #-}

instance
  ( sPowered ~ 10^s
  , IsChType (Decimal64 p s)
  ) =>
  ToChType (Decimal64 p s) (Fixed sPowered) where
  toChType (MkFixed fixedRep) = MkDecimal64 (fromIntegral fixedRep)
  fromChType (MkDecimal64 int64) = MkFixed (fromIntegral int64)

instance KnownNat (10^s) => ToQueryPart (Decimal64 p s) where
  toQueryPart dec = byteString (BS8.pack $ show dec)


-- ** Decimal128

{- |
>>> chTypeName @(Decimal128 19 1)
"Decimal(19, 1)"
>>> toChType @(Decimal128 19 1) @(Fixed (10^1)) 1000.1
1000.1
>>> toChType @(Decimal128 19 5) @(Fixed (10^5)) 1000.1
1000.10000
-}
newtype Decimal128 (p :: Nat) (s :: Nat) = MkDecimal128 Int128

instance KnownNat (10^s) => Show (Decimal128 p s) where
  show (MkDecimal128 int128) = show $ MkFixed @_ @(10^s)(fromIntegral int128)
deriving newtype instance KnownNat (10^s) => Eq (Decimal128 p s)
deriving newtype instance KnownNat (10^s) => Ord (Decimal128 p s)

instance
  (ValidRanges 128 19 38 p s, KnownNat p, KnownNat s, KnownNat (10 ^ s))
  =>
  IsChType (Decimal128 p s) where
  chTypeName =
    let p = show (natVal @p Proxy)
        s = show (natVal @s Proxy)
    in "Decimal(" <> p <> ", "<> s <> ")"
  defaultValueOfTypeName = MkDecimal128 0

instance Serializable (Decimal128 p s) where
  serialize rev (MkDecimal128 int) = serialize @Int128 rev int
  deserialize rev = MkDecimal128 <$> deserialize @Int128 rev
  {-# INLINE deserialize #-}

instance
  ( sPowered ~ 10^s
  , IsChType (Decimal128 p s)
  ) =>
  ToChType (Decimal128 p s) (Fixed sPowered) where
  toChType (MkFixed fixedRep) = MkDecimal128 (fromIntegral fixedRep)
  fromChType (MkDecimal128 int128) = MkFixed (fromIntegral int128)

instance KnownNat (10^s) => ToQueryPart (Decimal128 p s) where
  toQueryPart dec = "'" <> byteString (BS8.pack $ show dec) <> "'"


-- ** Decimal256

{- |
>>> chTypeName @(Decimal256 39 1)
"Decimal(39, 1)"
>>> toChType @(Decimal256 39 1) @(Fixed (10^1)) 1000.1
1000.1
>>> toChType @(Decimal256 39 5) @(Fixed (10^5)) 1000.1
1000.10000
-}
newtype Decimal256 (p :: Nat) (s :: Nat) = MkDecimal256 Int256

instance KnownNat (10^s) => Show (Decimal256 p s) where
  show (MkDecimal256 int256) = show $ MkFixed @_ @(10^s)(fromIntegral int256)
deriving newtype instance KnownNat (10^s) => Eq (Decimal256 p s)
deriving newtype instance KnownNat (10^s) => Ord (Decimal256 p s)

instance
  (ValidRanges 256 39 76 p s, KnownNat p, KnownNat s, KnownNat (10 ^ s))
  =>
  IsChType (Decimal256 p s) where
  chTypeName =
    let p = show (natVal @p Proxy)
        s = show (natVal @s Proxy)
    in "Decimal(" <> p <> ", "<> s <> ")"
  defaultValueOfTypeName = MkDecimal256 0

instance Serializable (Decimal256 p s) where
  serialize rev (MkDecimal256 int) = serialize @Int256 rev int
  deserialize rev = MkDecimal256 <$> deserialize @Int256 rev
  {-# INLINE deserialize #-}

instance
  ( sPowered ~ 10^s
  , IsChType (Decimal256 p s)
  ) =>
  ToChType (Decimal256 p s) (Fixed sPowered) where
  toChType (MkFixed fixedRep) = MkDecimal256 (fromIntegral fixedRep)
  fromChType (MkDecimal256 int256) = MkFixed (fromIntegral int256)

instance KnownNat (10^s) => ToQueryPart (Decimal256 p s) where
  toQueryPart dec = "'" <> byteString (BS8.pack $ show dec) <> "'"


-- Range validations

type family ValidRanges (size :: Nat) (pMin :: Nat) (pMax :: Nat) (p :: Nat) (s :: Nat) :: Constraint
  where
  ValidRanges size pMin pMax p s =
    If (p >=? 0 && s <=? p)
      (
        If
          (pMin <=? p && p <=? pMax)
          (() :: Constraint)
          (TypeError
            (    'Text "Precision (p=" :<>: ShowType p :<>: 'Text ") should satisfy "
            :<>: ShowType pMin :<>: 'Text " <= p <= " :<>: ShowType pMax
            :<>: 'Text " for " :<>: DecimalType size
            )
          )
      )
      (TypeError
        (    'Text "Scale (s=" :<>: ShowType s :<>: 'Text ") and "
        :<>: 'Text "precision (p=" :<>: ShowType p :<>: 'Text ") "
        :<>: 'Text "should satisfy 0 <= s <= p for "
        :<>: DecimalType size
        )
      )

type DecimalType (size :: Nat) = 'Text "Decimal" :<>: ShowType size :<>: 'Text " type"
