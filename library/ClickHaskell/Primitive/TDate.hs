module ClickHaskell.Primitive.TDate where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Data.ByteString.Builder (word16LE)
import Data.Word (Word16)
import Data.Bits (Bits)
import Control.DeepSeq (NFData)

-- External
import Data.Binary.Get (getWord16le)


{- | ClickHouse Date column type -}
newtype Date = MkDate Word16
  deriving newtype (Show, Eq, Bits, Bounded, Enum, NFData, Num)

instance IsChType Date where
  chTypeName = "Date"
  defaultValueOfTypeName = 0

instance Serializable Date where
  serialize _ (MkDate w16) = word16LE w16
  deserialize _ = MkDate <$> getWord16le
  {-# INLINE deserialize #-}

instance ToChType Date Word16 where
  toChType = MkDate
  fromChType (MkDate w16) = w16
