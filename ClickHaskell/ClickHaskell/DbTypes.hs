{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , LambdaCase
  , OverloadedStrings
#-}

{-# OPTIONS_GHC
  -Wno-missing-methods
#-}

module ClickHaskell.DbTypes
( IsChType(ToChTypeName, chTypeName)
, ToChType(toChType)
, FromChType(fromChType)
, ToQueryPart(toQueryPart)
, Serializable(..)
, Deserializable(..)
, ProtocolRevision

, ChDateTime

, ChInt8, ChInt16, ChInt32, ChInt64, ChInt128
, ChUInt8, ChUInt16, ChUInt32, ChUInt64, ChUInt128

, ChString
, ChUUID

, ChArray
, Nullable
, LowCardinality, IsLowCardinalitySupported

, UVarInt
, module Data.WideWord
) where


-- External
import Data.WideWord (Int128 (..), Word128(Word128))

-- GHC included
import Control.DeepSeq (NFData)
import Data.Binary.Get
import Data.Binary.Get.Internal (readN)
import Data.Binary.Put
import Data.Bits (Bits (..))
import Data.ByteString as BS (StrictByteString, length, take, toStrict)
import Data.ByteString.Builder as BS (Builder, byteString, word8, toLazyByteString)
import Data.ByteString.Char8 as BS8 (concatMap, length, pack, replicate, singleton)
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (uncons)
import Data.String (IsString)
import Data.Text as Text (Text)
import Data.Text.Encoding as Text (encodeUtf8)
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..))
import Data.Vector.Primitive.Mutable (Prim)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.TypeLits (AppendSymbol, ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


class
  IsChType chType
  where
  -- | Shows database original type name
  --
  -- @
  -- type ToChTypeName ChString = \"String\"
  -- type ToChTypeName (Nullable ChUInt32) = \"Nullable(UInt32)\"
  -- @
  type ToChTypeName chType :: Symbol

  chTypeName :: KnownSymbol (ToChTypeName chType) => Builder
  chTypeName = byteString . BS8.pack . symbolVal @(ToChTypeName chType) $ Proxy

class IsChType chType => ToChType chType inputType    where toChType    :: inputType -> chType
class IsChType chType => FromChType chType outputType where fromChType  :: chType -> outputType
class IsChType chType => ToQueryPart chType           where toQueryPart :: chType -> BS.Builder




-- | ClickHouse Nullable(T) column type
-- (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

instance {-# OVERLAPPING #-}
  ( TypeError
    (     'Text (ToChTypeName (Nullable (LowCardinality chType))) ':<>: 'Text " is unsupported type in ClickHouse."
    ':$$: 'Text "Use " ':<>: 'Text (ToChTypeName (LowCardinality (Nullable chType))) ':<>: 'Text " instead."
    )
  , IsChType chType
  ) => IsChType (Nullable (LowCardinality chType))

instance
  IsChType chType
  =>
  IsChType (Nullable chType)
  where
  type ToChTypeName (Nullable chType) = NullableTypeName chType

instance
  ToQueryPart chType
  =>
  ToQueryPart (Nullable chType)
  where
  toQueryPart = maybe "null" toQueryPart

instance
  ToChType inputType chType
  =>
  ToChType (Nullable inputType) (Nullable chType)
  where
  toChType = fmap (toChType @inputType @chType)

instance
  FromChType chType inputType
  =>
  FromChType (Nullable chType) (Nullable inputType)
  where
  fromChType = fmap (fromChType @chType)




-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
deriving newtype instance (Eq chType, IsLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving newtype instance (Show chType, IsLowCardinalitySupported chType) => Show (LowCardinality chType)
deriving newtype instance (NFData chType, IsLowCardinalitySupported chType) => NFData (LowCardinality chType)
deriving newtype instance IsString (LowCardinality ChString)

class IsChType chType => IsLowCardinalitySupported chType
instance IsLowCardinalitySupported ChString
instance IsLowCardinalitySupported chType => IsLowCardinalitySupported (Nullable chType)

instance {-# OVERLAPPABLE #-}
  ( IsChType chType
  , TypeError
    (    'Text "LowCardinality("  ':<>: 'ShowType chType  ':<>: 'Text ") is unsupported"
    ':$$: 'Text "Use one of these types:"
    ':$$: 'Text "  ChString"
    ':$$: 'Text "  ChDateTime"
    ':$$: 'Text "  Nullable(T)"
    )
  ) => IsLowCardinalitySupported chType

instance
  IsLowCardinalitySupported chType
  =>
  IsChType (LowCardinality chType)
  where
  type ToChTypeName (LowCardinality chType) = "LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

instance
  ( ToChType inputType chType
  , IsLowCardinalitySupported inputType
  )
  =>
  ToChType (LowCardinality inputType) chType
  where
  toChType = MkLowCardinality . toChType

instance IsLowCardinalitySupported chType => ToChType chType (LowCardinality chType)
  where
  toChType (MkLowCardinality value) = value

instance IsLowCardinalitySupported chType => FromChType chType (LowCardinality chType)
  where
  fromChType = MkLowCardinality

instance
  ( FromChType chType outputType
  , IsLowCardinalitySupported chType
  )
  =>
  FromChType (LowCardinality chType) outputType
  where
  fromChType (MkLowCardinality value) = fromChType value

instance
  ( ToQueryPart chType
  , IsLowCardinalitySupported chType
  )
  =>
  ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType




-- | ClickHouse UUID column type
data ChUUID = MkChUUID {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Generic, Show, Eq, NFData)

instance IsChType ChUUID where type ToChTypeName ChUUID = "UUID"

instance Deserializable ChUUID where deserialize _ = MkChUUID <$> getWord64le <*> getWord64le
instance Serializable ChUUID where serialize _ = execPut . (\(hi, lo) -> putWord64le lo <> putWord64le hi) . fromChType

instance ToChType ChUUID ChUUID where toChType = id
instance ToChType ChUUID Word64 where toChType = MkChUUID 0
instance ToChType ChUUID (Word64, Word64) where toChType = uncurry MkChUUID

instance FromChType ChUUID ChUUID where fromChType = id
instance FromChType ChUUID (Word64, Word64) where fromChType (MkChUUID w64hi w64lo) = (w64hi, w64lo)




-- | ClickHouse String column type
newtype ChString = MkChString StrictByteString
  deriving newtype (Show, Eq, IsString, NFData)

instance IsChType ChString where type ToChTypeName ChString = "String"

instance Serializable ChString where
  serialize rev str
    =  (serialize @UVarInt rev . fromIntegral . BS.length . fromChType) str
    <> (execPut . putByteString . fromChType) str

instance Deserializable ChString where
  deserialize rev = do
    strSize <- fromIntegral <$> deserialize @UVarInt rev
    toChType <$> readN strSize (BS.take strSize)

instance ToQueryPart ChString where toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"

escapeQuery :: StrictByteString -> Builder
escapeQuery -- [ClickHaskell.DbTypes.ToDo.1]: Optimize
  = BS.byteString
  . BS8.concatMap (\case '\'' -> "\\\'"; '\\' -> "\\\\"; sym -> BS8.singleton sym;)

instance ToChType ChString ChString         where toChType = id
instance ToChType ChString StrictByteString where toChType = MkChString
instance ToChType ChString Builder          where toChType = MkChString . toStrict . toLazyByteString
instance ToChType ChString String           where toChType = MkChString . BS8.pack
instance ToChType ChString Text             where toChType = MkChString . Text.encodeUtf8
instance ToChType ChString Int              where toChType = MkChString . BS8.pack . show

instance FromChType ChString ChString         where fromChType = id
instance FromChType ChString StrictByteString where fromChType (MkChString string) = string
instance
  ( TypeError
    (     'Text "ChString to Text using FromChType convertion could cause exception"
    ':$$: 'Text "Decode ByteString manually if you are sure it's always can be decoded or replace it with ByteString"
    )
  ) =>
  FromChType ChString Text




-- | ClickHouse Int8 column type
newtype ChInt8 = MkChInt8 Int8
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt8 where type ToChTypeName ChInt8 = "Int8"

instance Serializable ChInt8 where serialize _ = execPut . putInt8 . fromChType
instance Deserializable ChInt8 where deserialize _ = toChType <$> getInt8

instance ToQueryPart ChInt8
  where
  toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt8 ChInt8 where toChType = id
instance ToChType ChInt8 Int8   where toChType = MkChInt8

instance FromChType ChInt8 ChInt8 where fromChType = id
instance FromChType ChInt8 Int8   where fromChType = coerce




-- | ClickHouse Int16 column type
newtype ChInt16 = MkChInt16 Int16
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt16 where type ToChTypeName ChInt16 = "Int16"

instance Serializable ChInt16 where serialize _ = execPut . putInt16le . fromChType
instance Deserializable ChInt16 where deserialize _ = toChType <$> getInt16le

instance ToQueryPart ChInt16 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt16 ChInt16 where toChType = id
instance ToChType ChInt16 Int16   where toChType = MkChInt16

instance FromChType ChInt16 ChInt16 where fromChType = id
instance FromChType ChInt16 Int16   where fromChType (MkChInt16 int16) = int16




-- | ClickHouse Int32 column type
newtype ChInt32 = MkChInt32 Int32
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt32 where type ToChTypeName ChInt32 = "Int32"

instance Serializable ChInt32 where serialize _ = execPut . putInt32le . fromChType
instance Deserializable ChInt32 where deserialize _ = toChType <$> getInt32le

instance ToQueryPart ChInt32 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt32 ChInt32 where toChType = id
instance ToChType ChInt32 Int32   where toChType = MkChInt32

instance FromChType ChInt32 ChInt32 where fromChType = id
instance FromChType ChInt32 Int32   where fromChType (MkChInt32 int32) = int32




-- | ClickHouse Int64 column type
newtype ChInt64 = MkChInt64 Int64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt64 where type ToChTypeName ChInt64 = "Int64"

instance Serializable ChInt64 where serialize _ = execPut . putInt64le . fromChType
instance Deserializable ChInt64 where deserialize _ = toChType <$> getInt64le

instance ToQueryPart ChInt64 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt64 ChInt64 where toChType = id
instance ToChType ChInt64 Int64   where toChType = MkChInt64 . fromIntegral
instance ToChType ChInt64 Int     where toChType = MkChInt64 . fromIntegral

instance FromChType ChInt64 ChInt64 where fromChType = id
instance FromChType ChInt64 Int64   where fromChType = coerce




-- | ClickHouse Int128 column type
newtype ChInt128 = MkChInt128 Int128
  deriving newtype (Show, Eq, Num, Prim, Bits, Ord, Real, Enum, Integral, Bounded, NFData)

instance IsChType ChInt128 where type ToChTypeName ChInt128 = "Int128"

instance Serializable ChInt128 where serialize _ = execPut . (\(Int128 hi lo) -> putWord64le hi <> putWord64le lo) . fromChType
instance Deserializable ChInt128 where deserialize _ = toChType <$> (Int128 <$> getWord64le <*> getWord64le)

instance ToQueryPart ChInt128 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt128 ChInt128 where toChType = id
instance ToChType ChInt128 Int128   where toChType = MkChInt128 . fromIntegral

instance FromChType ChInt128 ChInt128 where fromChType = id
instance FromChType ChInt128 Int128   where fromChType (MkChInt128 int128) = int128




-- | ClickHouse UInt8 column type
newtype ChUInt8 = MkChUInt8 Word8
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt8 where type ToChTypeName ChUInt8 = "UInt8"

instance Serializable ChUInt8 where serialize _ = execPut . putWord8 . fromChType
instance Deserializable ChUInt8 where deserialize _ = toChType <$> getWord8

instance ToQueryPart ChUInt8 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt8 ChUInt8 where toChType = id
instance ToChType ChUInt8 Word8   where toChType = MkChUInt8

instance FromChType ChUInt8 ChUInt8 where fromChType = id
instance FromChType ChUInt8 Word8   where fromChType (MkChUInt8 w8) = w8




-- | ClickHouse UInt16 column type
newtype ChUInt16 = MkChUInt16 Word16
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt16 where type ToChTypeName ChUInt16 = "UInt16"

instance Serializable ChUInt16 where serialize _ = execPut . putWord16le . fromChType
instance Deserializable ChUInt16 where deserialize _ = toChType <$> getWord16le

instance ToQueryPart ChUInt16 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt16 ChUInt16 where toChType = id
instance ToChType ChUInt16 Word16   where toChType = coerce

instance FromChType ChUInt16 ChUInt16 where fromChType = id
instance FromChType ChUInt16 Word16   where fromChType = coerce




-- | ClickHouse UInt32 column type
newtype ChUInt32 = MkChUInt32 Word32
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt32 where type ToChTypeName ChUInt32 = "UInt32"

instance Serializable ChUInt32 where serialize _ = execPut . putWord32le . fromChType
instance Deserializable ChUInt32 where deserialize _ = toChType <$> getWord32le

instance ToQueryPart ChUInt32 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt32 ChUInt32 where toChType = id
instance ToChType ChUInt32 Word32   where toChType = MkChUInt32

instance FromChType ChUInt32 ChUInt32 where fromChType = id
instance FromChType ChUInt32 Word32   where fromChType (MkChUInt32 word32) = word32




-- | ClickHouse UInt64 column type
newtype ChUInt64 = MkChUInt64 Word64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt64 where type ToChTypeName ChUInt64 = "UInt64"

instance Serializable ChUInt64 where serialize _ = execPut . putWord64le . fromChType
instance Deserializable ChUInt64 where deserialize _ = toChType <$> getWord64le

instance ToQueryPart ChUInt64 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt64 ChUInt64 where toChType = id
instance ToChType ChUInt64 Word64   where toChType = MkChUInt64

instance FromChType ChUInt64 ChUInt64 where fromChType = id
instance FromChType ChUInt64 Word64   where fromChType (MkChUInt64 w64) = w64




-- | ClickHouse UInt128 column type
newtype ChUInt128 = MkChUInt128 Word128
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt128 where type ToChTypeName ChUInt128 = "UInt128"

instance Serializable ChUInt128 where serialize _ = execPut . (\(Word128 hi lo) -> putWord64le hi <> putWord64le lo) . fromChType
instance Deserializable ChUInt128 where deserialize _ = toChType <$> (Word128 <$> getWord64le <*> getWord64le)

instance ToQueryPart ChUInt128 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt128 ChUInt128 where toChType = id
instance ToChType ChUInt128 Word128   where toChType = MkChUInt128
instance ToChType ChUInt128 Word64    where toChType = MkChUInt128 . fromIntegral

instance FromChType ChUInt128 ChUInt128 where fromChType = id
instance FromChType ChUInt128 Word128   where fromChType (MkChUInt128 w128) = w128




-- | ClickHouse DateTime column type
newtype ChDateTime = MkChDateTime Word32
  deriving newtype (Show, Eq, Prim, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChDateTime where type ToChTypeName ChDateTime = "DateTime"

instance Serializable ChDateTime where serialize _ = execPut . putWord32le . fromChType
instance Deserializable ChDateTime where deserialize _ = toChType <$> getWord32le

instance ToQueryPart ChDateTime
  where
  toQueryPart chDateTime = let time = BS8.pack . show . fromChType @ChDateTime @Word32 $ chDateTime
    in BS.byteString (BS8.replicate (10 - BS8.length time) '0' <> time)

instance ToChType ChDateTime ChDateTime where toChType = id
instance ToChType ChDateTime Word32     where toChType = MkChDateTime
instance ToChType ChDateTime UTCTime    where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType ChDateTime ZonedTime  where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC

instance FromChType ChDateTime ChDateTime where fromChType = id
instance FromChType ChDateTime Word32     where fromChType = coerce
instance FromChType ChDateTime UTCTime    where fromChType (MkChDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)




newtype ChDate = MkChDate Word16
  deriving newtype (Show, Eq, Prim, Bits, Bounded, Enum, NFData)

instance IsChType ChDate where type ToChTypeName ChDate = "Date"

instance Serializable ChDate where serialize _ = execPut . putWord16le . fromChType
instance Deserializable ChDate where deserialize _ = toChType <$> getWord16le

instance ToChType ChDate ChDate where toChType = id
instance ToChType ChDate Word16 where toChType = MkChDate

instance FromChType ChDate ChDate where fromChType = id
instance FromChType ChDate Word16 where fromChType = coerce




newtype ChArray a = MkChArray [a]
  deriving newtype (Show, Eq, NFData)

instance IsChType chType => IsChType (ChArray chType)
  where
  type ToChTypeName    (ChArray chType) = "Array(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

instance ToQueryPart chType => ToQueryPart (ChArray chType)
  where
  toQueryPart
    = (\x -> "[" <> x <> "]")
    . (maybe "" (uncurry (foldr (\ a b -> a <> "," <> b)))
    . uncons
    . map (toQueryPart @chType))
    . fromChType 

instance IsChType chType => FromChType (ChArray chType) [chType] where fromChType (MkChArray values) = values

instance IsChType chType           => ToChType (ChArray chType) [chType] where toChType = MkChArray
instance ToChType chType inputType => ToChType (ChArray chType) [inputType] where toChType = MkChArray . map toChType




{- |
  Unsigned variable-length quantity encoding
  
  Part of protocol implementation
-}
newtype UVarInt = MkUVarInt Word64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance Deserializable UVarInt where
  deserialize _ = go 0 (0 :: UVarInt)
    where
    go i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else go (i + 1) $! o'
    go _ _ = fail "input exceeds varuint size"

instance Serializable UVarInt where
  serialize _ = go
    where
    go i
      | i < 0x80 = word8 (fromIntegral i)
      | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)



-- * (De)serialization

type ProtocolRevision = UVarInt

-- ** Deserializable

class
  Deserializable chType
  where
  default deserialize :: (Generic chType, GDeserializable (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev

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


-- ** Serializable

class Serializable chType
  where
  default serialize :: (Generic chType, GSerializable (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from

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
