{-# LANGUAGE
    OverloadedStrings
#-}
module ClickHaskell.Serialization where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Versioning

-- GHC included
import Data.Binary.Put
import Data.Bits (Bits (..))
import Data.ByteString as BS (length)
import Data.ByteString.Builder as BS (Builder, word8)
import Data.Coerce (coerce)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, KnownNat)

-- * Serializable

class Serializable chType
  where
  default serialize :: (Generic chType, GSerializable (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from

-- * Versioning

instance
  ( KnownNat revision
  , Serializable chType
  )
  =>
  Serializable (SinceRevision chType revision)
  where
  serialize rev (MkSinceRevision val) = afterRevision @revision rev (serialize rev val)
  serialize rev NotPresented          = afterRevision @revision rev (error "Unexpected error")

instance Serializable ProtocolRevision where
  serialize rev = serialize @UVarInt rev . coerce


-- * Database types
instance Serializable UVarInt where
  serialize _ = go
    where
    go i
      | i < 0x80 = word8 (fromIntegral i)
      | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)

instance Serializable ChString where
  serialize rev str
    =  (serialize @UVarInt rev . fromIntegral . BS.length . fromChType) str
    <> (execPut . putByteString . fromChType) str

instance Serializable ChUUID where serialize _ = execPut . (\(hi, lo) -> putWord64le lo <> putWord64le hi) . fromChType
instance Serializable ChInt8 where serialize _ = execPut . putInt8 . fromChType
instance Serializable ChInt16 where serialize _ = execPut . putInt16le . fromChType
instance Serializable ChInt32 where serialize _ = execPut . putInt32le . fromChType
instance Serializable ChInt64 where serialize _ = execPut . putInt64le . fromChType
instance Serializable ChInt128 where serialize _ = execPut . (\(Int128 hi lo) -> putWord64le hi <> putWord64le lo) . fromChType
instance Serializable ChUInt8 where serialize _ = execPut . putWord8 . fromChType
instance Serializable ChUInt16 where serialize _ = execPut . putWord16le . fromChType
instance Serializable ChUInt32 where serialize _ = execPut . putWord32le . fromChType
instance Serializable ChUInt64 where serialize _ = execPut . putWord64le . fromChType
instance Serializable ChUInt128 where serialize _ = execPut . (\(Word128 hi lo) -> putWord64le hi <> putWord64le lo) . fromChType
instance Serializable ChDateTime where serialize _ = execPut . putWord32le . fromChType
instance Serializable ChDate where serialize _ = execPut . putWord16le . fromChType

-- * Columns

instance
  Serializable (Columns '[])
  where
  {-# INLINE serialize #-}
  serialize _rev Empty = ""

instance
  ( Serializable (Columns columns)
  , Serializable (Column name chType)
  )
  =>
  Serializable (Columns (Column name chType ': columns))
  where
  {-# INLINE serialize #-}
  serialize rev (AddColumn col columns) = serialize rev col <> serialize rev columns

instance
  ( KnownColumn (Column name chType)
  , IsChType chType
  , KnownSymbol name
  , Serializable chType
  ) => Serializable (Column name chType) where
  {-# INLINE serialize #-}
  serialize rev (MkColumn _size values)
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) values)

-- * Generics

class GSerializable f
  where
  gSerialize :: ProtocolRevision -> f p -> Builder

instance
  GSerializable f
  =>
  GSerializable (D1 c (C1 c2 f))
  where
  {-# INLINE gSerialize #-}
  gSerialize rev (M1 (M1 re)) = gSerialize rev re

instance
  GSerializable (left1 :*: (left2 :*: right))
  =>
  GSerializable ((left1 :*: left2) :*: right)
  where
  {-# INLINE gSerialize #-}
  gSerialize rev ((l1 :*: l2) :*: r) = gSerialize rev (l1 :*: (l2 :*: r))

instance
  Serializable chType
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  {-# INLINE gSerialize #-}
  gSerialize rev = serialize rev . unK1 . unM1

instance
  (Serializable chType, GSerializable right)
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType) :*: right)
  where
  {-# INLINE gSerialize #-}
  gSerialize rev (left :*: right)
    = (serialize rev . unK1 . unM1 $ left) <> gSerialize rev right