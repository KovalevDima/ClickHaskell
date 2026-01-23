{-# OPTIONS_GHC -Wno-orphans #-}
module ClickHaskell.Primitive.TNullable where

-- Internal
import ClickHaskell.Primitive.Serialization
import ClickHaskell.Primitive.TUInt

-- GHC included
import Control.Monad (forM)


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


instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name (Nullable chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev rows f = do
    nulls <- replicateGet @UInt8 rev rows
    forM nulls (\nulFlag -> case nulFlag of
        0 -> f . Just <$> deserialize @chType rev
        _ -> (f Nothing <$ deserialize @chType rev)
      )

  {-# INLINE serializeColumn #-}
  serializeColumn rev f column
    =  foldMap (serialize @UInt8 rev . maybe 1 (const 0) . f) column
    <> foldMap (serialize @chType rev . maybe defaultValueOfTypeName id . f) column
