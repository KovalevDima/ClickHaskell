module ClickHaskell.Primitive.TArray where

-- Internal
import ClickHaskell.Primitive.Serialization
import ClickHaskell.Primitive.TUInt

-- GHC included
import Control.DeepSeq (NFData)
import Control.Monad (forM)
import Data.Coerce (coerce)
import Data.List (mapAccumL, uncons)
import GHC.TypeError
import GHC.TypeLits (KnownSymbol)


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

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Array chType))
  , Serializable chType
  )
  => SerializableColumn (Column name (Array chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows f = do
    offsets <- replicateGet @UInt64 rev rows
    let lengths = zipWith (-) offsets (0 : (init offsets))
    forM lengths (fmap (f . MkChArray) . replicateGet @chType rev . fromIntegral)

  {-# INLINE serializeColumn #-}
  serializeColumn rev f column
    =  foldMap (serialize @UInt64 rev) offsets
    <> foldMap (foldMap (serialize @chType rev) . f) column
    where
    offsets =
      snd $
        mapAccumL
          (\offset xs ->
            let nextOffset = offset + fromIntegral (length xs)
            in (nextOffset, nextOffset)
          )
          0
          (map f column)

instance {-# OVERLAPPING #-}
  ( KnownSymbol name
  , IsChType chType
  , TypeError ('Text "Nested Arrays types (column \"" :<>: 'Text name :<>: 'Text "\") are unsupported")
  )
  => SerializableColumn (Column name (Array (Array chType)))
  where
  deserializeColumn = error "Impossible"
  serializeColumn = error "Impossible"
