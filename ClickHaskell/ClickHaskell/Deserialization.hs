{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC
  -Wno-orphans
#-}
{-# LANGUAGE LambdaCase #-}

module ClickHaskell.Deserialization where

-- Internal dependencies
import ClickHaskell.Versioning (ProtocolRevision(..), SinceRevision (..), DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION)
import ClickHaskell.DbTypes

-- GHC included
import Data.Bits (Bits (..))
import Data.ByteString as BS (take)
import Data.Coerce (coerce)
import Data.Typeable (Proxy (..))
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal, TypeError, ErrorMessage (..))
import Control.Monad (replicateM, forM)
import Data.Binary.Get.Internal
import Data.Binary.Get

class
  Deserializable chType
  where
  default deserialize :: (Generic chType, GDeserializable (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev

-- * Generics

class GDeserializable f
  where
  gDeserialize :: ProtocolRevision -> Get (f p)

instance
  GDeserializable f
  =>
  GDeserializable (D1 c (C1 c2 f))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = M1 . M1 <$> gDeserialize rev

instance
  GDeserializable (left :*: (right1 :*: right2))
  =>
  GDeserializable ((left :*: right1) :*: right2)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = (\(l :*: (r1 :*: r2)) -> (l :*: r1) :*: r2) <$> gDeserialize rev

instance
  (GDeserializable (S1 metaSel field), GDeserializable right)
  =>
  GDeserializable (S1 metaSel field :*: right)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = (:*:) <$> gDeserialize rev <*> gDeserialize rev

instance
  Deserializable chType
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev =  M1 . K1 <$> deserialize @chType rev

-- * Versioning

instance
  ( KnownNat revision
  , Deserializable chType
  )
  =>
  Deserializable (SinceRevision chType revision)
  where
  deserialize rev =
    if rev >= (fromIntegral . natVal) (Proxy @revision)
    then MkSinceRevision <$> deserialize @chType rev
    else pure NotPresented

instance Deserializable ProtocolRevision where
  deserialize rev = coerce <$> deserialize @UVarInt rev


-- * Database types

instance Deserializable ChUUID where
  deserialize _ = MkChUUID <$> (flip Word128 <$> getWord64le <*> getWord64le)

instance Deserializable ChString where
  deserialize rev = do
    strSize <- fromIntegral <$> deserialize @UVarInt rev
    toChType <$> readN strSize (BS.take strSize)


instance Deserializable ChInt8 where deserialize _ = toChType <$> getInt8
instance Deserializable ChInt16 where deserialize _ = toChType <$> getInt16le
instance Deserializable ChInt32 where deserialize _ = toChType <$> getInt32le
instance Deserializable ChInt64 where deserialize _ = toChType <$> getInt64le
instance Deserializable ChInt128 where deserialize _ = toChType <$> (flip Int128 <$> getWord64le <*> getWord64le)
instance Deserializable ChUInt8 where deserialize _ = toChType <$> getWord8
instance Deserializable ChUInt16 where deserialize _ = toChType <$> getWord16le
instance Deserializable ChUInt32 where deserialize _ = toChType <$> getWord32le
instance Deserializable ChUInt64 where deserialize _ = toChType <$> getWord64le
instance Deserializable ChUInt128 where deserialize _ = toChType <$> (flip Word128 <$> getWord64le <*> getWord64le)
instance Deserializable ChDateTime where deserialize _ = toChType <$> getWord32le
instance Deserializable ChDate where deserialize _ = toChType <$> getWord16le

instance
  ( Deserializable chType
  , ToChType chType chType
  , TypeError ('Text "Arrays still unsupported to select")
  )
  => Deserializable (ChArray chType) where
  deserialize rev = do
    (arraySize, _offsets) <- readOffsets rev
    toChType <$> replicateM (fromIntegral arraySize) (deserialize @chType rev)
    where
    readOffsets :: ProtocolRevision -> Get (ChUInt64, [ChUInt64])
    readOffsets revivion = do
      size <- deserialize @ChUInt64 rev
      (size, ) <$> go size
      where
      go arraySize =
        do
        nextOffset <- deserialize @ChUInt64 revivion
        if arraySize == nextOffset
          then pure [nextOffset]
          else (nextOffset :) <$> go arraySize

instance Deserializable UVarInt where
  deserialize _ = go 0 (0 :: UVarInt)
    where
    go i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else go (i + 1) $! o'
    go _ _ = fail "input exceeds varuint size"


-- ** Columns deserialization

class DeserializableColumns columns where
  deserializeColumns :: ProtocolRevision -> UVarInt -> Get columns

instance
  DeserializableColumns (Columns '[])
  where
  {-# INLINE deserializeColumns #-}
  deserializeColumns _rev _rows = pure Empty

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  , DeserializableColumns (Columns extraColumns)
  )
  =>
  DeserializableColumns (Columns (Column name chType ': extraColumns))
  where
  {-# INLINE deserializeColumns #-}
  deserializeColumns rev rows = do
    AddColumn
      <$> (do
        _columnName <- deserialize @ChString rev
        _columnType <- deserialize @ChString rev
        _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
        column <- replicateM (fromIntegral rows) (deserialize @chType rev)
        pure $ MkColumn rows column
      )
      <*> deserializeColumns @(Columns extraColumns) rev rows

{-# SPECIALIZE replicateM :: Int -> Get chType -> Get [chType] #-}

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Deserializable chType
  , DeserializableColumns (Columns extraColumns)
  )
  =>
  DeserializableColumns (Columns (Column name (Nullable chType) ': extraColumns))
  where
  {-# INLINE deserializeColumns #-}
  deserializeColumns rev rows = do
    AddColumn
      <$> (do
        _columnName <- deserialize @ChString rev
        _columnType <- deserialize @ChString rev
        _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
        nulls <- replicateM (fromIntegral rows) (deserialize @ChUInt8 rev)
        nullable <-
          forM
            nulls
            (\case
              0 -> Just <$> deserialize @chType rev
              _ -> (Nothing <$ deserialize @chType rev)
            )
        pure $ MkColumn rows nullable
      )
      <*> deserializeColumns @(Columns extraColumns) rev rows
