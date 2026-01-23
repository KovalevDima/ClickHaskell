module ClickHaskell.Primitive.TArray where

-- Internal
import ClickHaskell.Primitive.Serialization

import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.List (uncons)


-- ** Array

-- | ClickHouse Array column type
newtype Array a = MkChArray [a]
  deriving newtype (Show, Eq, NFData, Foldable)
instance IsChType chType => IsChType (Array chType)
  where
  chTypeName = "Array(" <> chTypeName @chType <> ")"
  defaultValueOfTypeName = MkChArray []

instance ToChType chType inputType => ToChType (Array chType) [inputType]
  where
  toChType = MkChArray . map toChType
  fromChType (MkChArray values) = map fromChType values

instance (IsChType chType, ToQueryPart chType) => ToQueryPart (Array chType)
  where
  toQueryPart
    = (\x -> "[" <> x <> "]")
    . (maybe "" (uncurry (foldl (\a b -> a <> "," <> b))) . uncons
    . map (toQueryPart @chType)) . coerce @(Array chType) @[chType]
