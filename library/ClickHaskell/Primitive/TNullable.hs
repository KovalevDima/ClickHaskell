{-# OPTIONS_GHC -Wno-orphans #-}
module ClickHaskell.Primitive.TNullable where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included

-- External


{- | ClickHouse Nullable(T) column type
 (type synonym for Maybe)
 -}
type Nullable = Maybe
instance IsChType chType => IsChType (Nullable chType)
  where
  chTypeName = "Nullable(" <> chTypeName @chType <> ")"
  defaultValueOfTypeName = Nothing

instance
  ToChType inputType chType
  =>
  ToChType (Nullable inputType) (Nullable chType)
  where
  toChType = fmap (toChType @inputType @chType)
  fromChType = fmap (fromChType @inputType)

instance ToQueryPart chType => ToQueryPart (Nullable chType)
  where
  toQueryPart = maybe "null" toQueryPart
