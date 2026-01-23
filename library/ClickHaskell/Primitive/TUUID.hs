module ClickHaskell.Primitive.TUUID where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Control.DeepSeq (NFData)
import Data.Bits
import Data.ByteString.Builder (Builder, word16HexFixed, word64LE)
import Data.Word (Word64)

-- External
import Data.Binary.Get (getWord64le)
import Data.WideWord (Word128(..))


{- | ClickHouse UUID column type -}
newtype UUID = MkUUID Word128
  deriving newtype (Show, Eq, NFData, Bounded, Enum, Num)

instance IsChType UUID where
  chTypeName = "UUID"
  defaultValueOfTypeName = 0

instance Serializable UUID where
  serialize _ = (\(MkUUID (Word128 hi lo)) -> word64LE lo <> word64LE hi)
  deserialize _ = do
    low <- getWord64le
    high <- getWord64le
    pure $ MkUUID (Word128 high low)
  {-# INLINE deserialize #-}

instance ToChType UUID (Word64, Word64) where
  toChType = MkUUID . uncurry (flip Word128)
  fromChType (MkUUID (Word128 w64hi w64lo)) = (w64hi, w64lo)

instance ToQueryPart UUID where
  toQueryPart (MkUUID (Word128 hi lo)) = mconcat
    ["'", p 3 hi, p 2 hi, "-", p 1 hi, "-", p 0 hi, "-", p 3 lo, "-", p 2 lo, p 1 lo, p 0 lo, "'"]
    where
    p :: Int -> Word64 -> Builder
    p shiftN word = word16HexFixed $ fromIntegral (word `unsafeShiftR` (shiftN*16))
