{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DerivingStrategies
  , DuplicateRecordFields
  , OverloadedStrings
  , UndecidableInstances
#-}

module ClickHaskell.NativeProtocol.Serialization where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol.Versioning

-- GHC included
import Data.Binary.Get
import Data.Binary.Get.Internal (readN)
import Data.Bits (Bits (..))
import Data.ByteString as BS (length, take, StrictByteString)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Builder as BS
  ( byteString
  , int16LE, int32LE, int64LE, int8
  , word16LE, word32LE, word64LE, word8
  )
import Data.Typeable (Proxy (..))
import Data.WideWord (Int128 (..), Word128 (..))
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)
import ClickHaskell.Tables
import Data.ByteString.Lazy

-- * Serializable

class Serializable chType
  where
  default serialize :: (Generic chType, GSerializable (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from

instance Serializable UVarInt where
  serialize _ = go
    where
    go i
      | i < 0x80 = word8 (fromIntegral i)
      | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)
instance Serializable ChUInt8 where serialize _ = word8 . fromChType
instance Serializable ChUInt16 where serialize _ = word16LE . fromChType
instance Serializable ChUInt32 where serialize _ = word32LE . fromChType
instance Serializable ChUInt64 where serialize _ = word64LE . fromChType
instance Serializable ChUInt128 where serialize _ = (\(Word128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChInt8 where serialize _ = int8 . fromChType
instance Serializable ChInt16 where serialize _ = int16LE . fromChType
instance Serializable ChInt32 where serialize _ = int32LE . fromChType
instance Serializable ChInt64 where serialize _ = int64LE . fromChType
instance Serializable ChInt128 where serialize _ = (\(Int128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChString where
  serialize revision str
    =  (serialize @UVarInt revision . fromIntegral . BS.length . fromChType) str
    <> (BS.byteString . fromChType @_ @StrictByteString) str

class GSerializable f
  where
  gSerialize :: ProtocolRevision -> f p -> Builder

instance
  GSerializable f
  =>
  GSerializable (D1 c (C1 c2 f))
  where
  gSerialize rev (M1 (M1 re)) = gSerialize rev re

instance
  GSerializable (left1 :*: (left2 :*: right))
  =>
  GSerializable ((left1 :*: left2) :*: right)
  where
  gSerialize rev ((left :*: left2) :*: right)
    = gSerialize rev (left :*: (left2 :*: right))

instance
  Serializable chType
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  gSerialize rev = serialize rev . unK1 . unM1

instance
  (Serializable chType, GSerializable right)
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType) :*: right)
  where
  gSerialize rev (left :*: right)
    = (serialize rev . unK1 . unM1 $ left) <> gSerialize rev right


-- * Deserializable

class
  Deserializable chType
  where
  default deserialize :: (Generic chType, GDeserializable (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev

instance Deserializable UVarInt where
  deserialize _ = go 0 (0 :: UVarInt)
    where
    go i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else go (i + 1) $! o'
    go _ _ = fail "input exceeds varuint size"
instance Deserializable ChUInt8 where deserialize _ = toChType <$> getWord8
instance Deserializable ChUInt16 where deserialize _ = toChType <$> getWord16le
instance Deserializable ChUInt32 where deserialize _ = toChType <$> getWord32le
instance Deserializable ChUInt64 where deserialize _ = toChType <$> getWord64le
instance Deserializable ChUInt128 where deserialize _ = toChType <$> (Word128 <$> getWord64le <*> getWord64le)
instance Deserializable ChInt8 where deserialize _ = toChType <$> getInt8
instance Deserializable ChInt16 where deserialize _ = toChType <$> getInt16le
instance Deserializable ChInt32 where deserialize _ = toChType <$>  getInt32le
instance Deserializable ChInt64 where deserialize _ = toChType <$>  getInt64le
instance Deserializable ChInt128 where deserialize _ = toChType <$> (Int128 <$> getWord64le <*> getWord64le)
instance Deserializable ChString where
  deserialize revision = do
    strSize <- fromIntegral <$> deserialize @UVarInt revision
    toChType <$> readN strSize (BS.take strSize)

class GDeserializable f
  where
  gDeserialize :: ProtocolRevision -> Get (f p)

instance
  GDeserializable f
  =>
  GDeserializable (D1 c (C1 c2 f))
  where
  gDeserialize rev = M1 . M1 <$> gDeserialize rev

instance
  GDeserializable (left :*: (right1 :*: right2))
  =>
  GDeserializable ((left :*: right1) :*: right2)
  where
  gDeserialize rev =
    (\(left :*: (right1 :*: right2)) -> (left :*: right1) :*: right2)
    <$> gDeserialize rev

instance
  (Deserializable chType)
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  gDeserialize rev =  M1 . K1 <$> deserialize @chType rev

instance
  (Deserializable chType, GDeserializable right)
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType) :*: right)
  where
  gDeserialize rev = (:*:) <$> (M1 . K1 <$> deserialize @chType rev) <*> gDeserialize rev

instance {-# OVERLAPPING #-}
  GDeserializable right
  =>
  GDeserializable (S1 (MetaSel (Just "server_revision") a b f) (Rec0 UVarInt) :*: right)
  where
  gDeserialize rev = do
    server_revision <- deserialize @UVarInt rev
    (:*:) <$> (pure . M1 . K1 $ server_revision) <*> gDeserialize server_revision

-- * Columns serialization

instance
  ( CompiledColumn (Column name chType)
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name chType) where
  serialize rev (MkColumn values)
    =  serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnType @(Column name chType))
    <> (serialize @ChUInt32 rev . fromIntegral) (Prelude.length values)
    <> mconcat (Prelude.map (serialize @chType rev) values)


-- * Versionized serialization

instance
  ( KnownNat revision
  , Serializable chType
  )
  =>
  Serializable (SinceRevision chType revision)
  where
  serialize rev (MkSinceRevision val) = afterRevision @revision rev (serialize rev val)
  serialize rev NotPresented = afterRevision @revision rev (error "Unexpected error")

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
