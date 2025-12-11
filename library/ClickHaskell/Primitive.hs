{-# LANGUAGE BangPatterns #-}
module ClickHaskell.Primitive where

-- Internal
import Paths_ClickHaskell (version)

-- GHC included
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Binary.Get
import Data.Bits (Bits (setBit, unsafeShiftL, unsafeShiftR, (.&.), (.|.)))
import Data.Bool (bool)
import Data.ByteString as BS (ByteString, length)
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS8 (pack, unpack, concatMap, singleton, replicate, length)
import Data.ByteString.Lazy (toStrict)
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (uncons)
import Data.String (IsString (..))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.Type.Bool (Not)
import Data.Type.Equality (type (==))
import Data.Typeable (Proxy (..))
import Data.Version (Version (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (C1, D1, Generic (..), K1 (K1), M1 (M1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.TypeLits (ErrorMessage (..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal)
import Prelude hiding (liftA2)

-- External
import Data.WideWord (Int128 (..), Word256(..), Word128(..))

-- * User types

-- ** Abstractions

class Serializable chType
  where
  default serialize :: (Generic chType, GSerial (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from

  {-# INLINE deserialize #-}
  default deserialize :: (Generic chType, GSerial (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev

{-# INLINE replicateGet #-}
replicateGet :: Serializable chType => ProtocolRevision -> UVarInt -> Get [chType]
replicateGet rev cnt0 = loopGet cnt0
  where
  loopGet cnt
    | cnt == 0  = pure []
    | otherwise = liftA2 (:) (deserialize rev) (loopGet (cnt - 1))

instance Serializable prim => Serializable [prim] where
  serialize rev list
    =  serialize @UVarInt rev (fromIntegral $ Prelude.length list)
    <> foldMap (serialize @prim rev) list
  deserialize rev = do
    len <- deserialize @UVarInt rev
    replicateGet @prim rev len
  {-# INLINE deserialize #-}

class ToQueryPart chType where
  toQueryPart :: chType -> Builder

class IsChType chType
  where
  -- | Shows database original type name
  --
  -- @
  -- chTypeName \@ChString = \"String\"
  -- chTypeName \@(Nullable UInt32) = \"Nullable(UInt32)\"
  -- @
  chTypeName :: String

  defaultValueOfTypeName :: chType

class ToChType chType userType    where
  toChType   :: userType -> chType
  fromChType :: chType -> userType

instance {-# OVERLAPPABLE #-} (IsChType chType, chType ~ inputType) => ToChType chType inputType where
  toChType = id
  fromChType = id


-- ** Int8

instance IsChType Int8 where
  chTypeName = "Int8"
  defaultValueOfTypeName = 0

instance Serializable Int8 where
  serialize _ = int8
  deserialize _ = getInt8
  {-# INLINE deserialize #-}

instance ToQueryPart Int8 where
  toQueryPart = byteString . BS8.pack . show


-- ** Int16

instance IsChType Int16 where
  chTypeName = "Int16"
  defaultValueOfTypeName = 0

instance Serializable Int16 where
  serialize _ = int16LE
  deserialize _ = getInt16le
  {-# INLINE deserialize #-}

instance ToQueryPart Int16 where
  toQueryPart = byteString . BS8.pack . show


-- ** Int32

instance IsChType Int32 where
  chTypeName = "Int32"
  defaultValueOfTypeName = 0

instance Serializable Int32 where
  serialize _ = int32LE
  deserialize _ = getInt32le
  {-# INLINE deserialize #-}

instance ToQueryPart Int32 where
  toQueryPart = byteString . BS8.pack . show


-- ** Int64

instance IsChType Int64 where
  chTypeName = "Int64"
  defaultValueOfTypeName = 0

instance Serializable Int64 where
  serialize _ = int64LE
  deserialize _ = getInt64le
  {-# INLINE deserialize #-}

instance ToQueryPart Int64 where
  toQueryPart = byteString . BS8.pack . show


-- ** Int128

instance IsChType Int128 where
  chTypeName = "Int128"
  defaultValueOfTypeName = 0

instance Serializable Int128 where
  serialize _ = (\(Int128 hi lo) -> word64LE lo <> word64LE hi)
  deserialize _ = do
    low <- getWord64le
    high <- getWord64le
    pure $ Int128 high low
  {-# INLINE deserialize #-}

instance ToQueryPart Int128 where
  toQueryPart = byteString . BS8.pack . show


-- ** UInt8

{- | ClickHouse UInt8 column type -}
type UInt8 = Word8
instance IsChType UInt8 where
  chTypeName = "UInt8"
  defaultValueOfTypeName = 0

instance Serializable UInt8 where
  serialize _ = word8
  deserialize _ = getWord8
  {-# INLINE deserialize #-}

instance ToQueryPart UInt8 where
  toQueryPart = byteString . BS8.pack . show


-- ** UInt16

{- | ClickHouse UInt16 column type -}
type UInt16 = Word16
instance IsChType UInt16 where
  chTypeName = "UInt16"
  defaultValueOfTypeName = 0

instance Serializable UInt16 where
  serialize _ = word16LE
  deserialize _ = getWord16le
  {-# INLINE deserialize #-}

instance ToQueryPart UInt16 where
  toQueryPart = byteString . BS8.pack . show


-- ** UInt32

{- | ClickHouse UInt32 column type -}
type UInt32 = Word32
instance IsChType UInt32 where
  chTypeName = "UInt32"
  defaultValueOfTypeName = 0

instance Serializable UInt32 where
  serialize _ = word32LE
  deserialize _ = getWord32le
  {-# INLINE deserialize #-}

instance ToQueryPart UInt32 where
  toQueryPart = byteString . BS8.pack . show


-- ** UInt64

{- | ClickHouse UInt64 column type -}
type UInt64 = Word64
instance IsChType UInt64 where
  chTypeName = "UInt64"
  defaultValueOfTypeName = 0

instance Serializable UInt64 where
  serialize _ = word64LE
  deserialize _ = getWord64le
  {-# INLINE deserialize #-}

instance ToQueryPart UInt64 where
  toQueryPart = byteString . BS8.pack . show


-- ** UInt128

{- | ClickHouse UInt128 column type -}
type UInt128 = Word128
instance IsChType UInt128 where
  chTypeName = "UInt128"
  defaultValueOfTypeName = 0

instance Serializable UInt128 where
  serialize _ = (\(Word128 hi lo) -> word64LE lo <> word64LE hi)
  deserialize _ = do
    low <- getWord64le
    high <- getWord64le
    pure $ Word128 high low
  {-# INLINE deserialize #-}

instance ToQueryPart UInt128 where
  toQueryPart w128 = "'" <> (byteString . BS8.pack . show) w128 <> "'"


-- ** Int256

{- | ClickHouse UInt128 column type -}
type UInt256 = Word256
instance IsChType UInt256 where
  chTypeName = "UInt256"
  defaultValueOfTypeName = 0

instance Serializable UInt256 where
  serialize _ = (\(Word256 high mid1 mid0 low) -> word64LE low <> word64LE mid0 <> word64LE mid1 <> word64LE high)
  deserialize _ = do
    low <- getWord64le
    mid0 <- getWord64le
    mid1 <- getWord64le
    high <- getWord64le
    pure $ Word256 high mid1 mid0 low
  {-# INLINE deserialize #-}

instance ToQueryPart UInt256 where
  toQueryPart w256 = "'" <> (byteString . BS8.pack . show) w256 <> "'"


-- ** Date

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


-- ** ChString

{- | ClickHouse String column type -}
newtype ChString = MkChString BS.ByteString
  deriving newtype (Show, Eq, IsString, NFData)

instance IsChType ChString where
  chTypeName = "String"
  defaultValueOfTypeName = ""

instance Serializable ChString where
  serialize rev (MkChString str) = (serialize @UVarInt rev . fromIntegral . BS.length) str <> byteString str
  deserialize rev = do
    len <- deserialize @UVarInt rev
    MkChString <$> (getByteString . fromIntegral) len
  {-# INLINE deserialize #-}

instance ToChType ChString BS.ByteString where
  toChType = MkChString
  fromChType (MkChString string) = string

instance ToChType ChString Builder where
  toChType = MkChString . toStrict . toLazyByteString
  fromChType (MkChString string) = byteString string

instance ToChType ChString String where
  toChType = MkChString . BS8.pack
  fromChType (MkChString bs)= BS8.unpack bs

instance ToQueryPart ChString where
  toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"
    where
    escapeQuery :: BS.ByteString -> Builder
    escapeQuery = byteString . BS8.concatMap (\sym ->
      case sym of
        '\'' -> "\\\'"
        '\\' -> "\\\\"
        _ -> BS8.singleton sym
      )



-- ** UUID

{- | ClickHouse UUID column type -}
newtype UUID = MkUUID Word128
  deriving newtype (Generic, Show, Eq, NFData, Bounded, Enum, Num)
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


-- ** Nullable

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


-- ** Enum8

newtype Enum8 (enums :: Symbol) = MkEnum8 Int8
  deriving newtype (Serializable, Show, Eq, Num, Bits, Bounded, Enum)

instance KnownSymbol enums => IsChType (Enum8 enums) where
  chTypeName = "Enum8(" <> symbolVal @enums Proxy <> ")"
  defaultValueOfTypeName = 0

instance ToChType (Enum8 enums) Int8 where
  toChType = MkEnum8
  fromChType (MkEnum8 i8)= i8

instance ToQueryPart (Enum8 enums) where
  toQueryPart = toQueryPart . fromChType @_ @Int8


-- ** Enum16

newtype Enum16 (enums :: Symbol) = MkEnum16 Int16
  deriving newtype (Serializable, Show, Eq, Num, Bits, Bounded, Enum)

instance KnownSymbol enums => IsChType (Enum16 enums) where
  chTypeName = "Enum16(" <> symbolVal @enums Proxy <> ")"
  defaultValueOfTypeName = 0

instance ToChType (Enum16 enums) Int16 where
  toChType = MkEnum16
  fromChType (MkEnum16 i16) = i16

instance ToQueryPart (Enum16 enums) where
  toQueryPart = toQueryPart . fromChType @_ @Int16


-- ** Bool

instance IsChType Bool where
  chTypeName = "Bool"
  defaultValueOfTypeName = False

instance Serializable Bool where
  serialize _ = int8 . bool 0 1
  deserialize _ = (\int -> case int of 0->False; _->True) <$> getInt8
  {-# INLINE deserialize #-}

instance ToQueryPart Bool where
  toQueryPart = bool "false" "true"


-- ** DateTime

{- |
ClickHouse DateTime column type (parametrized with timezone)

>>> chTypeName @(DateTime "")
"DateTime"
>>> chTypeName @(DateTime "UTC")
"DateTime('UTC')"

__Note:__ 'DateTime' stores whole seconds only, so converting from 'UTCTime' \
will drop any sub-second precision.

>>> let myUtcTime = posixSecondsToUTCTime 0.042_042
>>> toChType @(DateTime "") @UTCTime myUtcTime
0
-}
newtype DateTime (tz :: Symbol) = MkDateTime Word32
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance KnownSymbol tz => IsChType (DateTime tz)
  where
  chTypeName = case symbolVal @tz Proxy of
    "" -> "DateTime"
    tz -> "DateTime('" <> tz <> "')"
  defaultValueOfTypeName = MkDateTime 0

instance Serializable (DateTime tz) where
  serialize _ (MkDateTime w32) = word32LE w32
  deserialize _ = MkDateTime <$> getWord32le
  {-# INLINE deserialize #-}

instance ToChType (DateTime tz) Word32     where
  toChType = MkDateTime
  fromChType (MkDateTime w32)= w32

instance ToChType (DateTime tz) UTCTime    where
  toChType = MkDateTime . floor . utcTimeToPOSIXSeconds
  fromChType (MkDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)

instance ToQueryPart (DateTime tz)
  where
  toQueryPart chDateTime = let time = BS8.pack . show . coerce @(DateTime tz) @Word32 $ chDateTime
    in byteString (BS8.replicate (10 - BS8.length time) '0' <> time)


-- ** DateTime64

{- |
ClickHouse DateTime64 column type (parametrized with timezone)

>>> chTypeName @(DateTime64 3 "")
"DateTime64(3)"
>>> chTypeName @(DateTime64 3 "UTC")
"DateTime64(3, 'UTC')"

__Note:__ conversion from 'UTCTime' may lose sub-second precision if \
the @precision@ parameter is lower than the actual timestamp precision.

>>> let myUtcTime = posixSecondsToUTCTime 42.000_000_042
>>> toChType @(DateTime64 6 "") @UTCTime myUtcTime
42000000
>>> toChType @(DateTime64 9 "") @UTCTime myUtcTime
42000000042
-}
newtype DateTime64 (precision :: Nat) (tz :: Symbol) = MkDateTime64 Word64
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance
  (KnownSymbol tz, KnownNat precision)
  =>
  IsChType (DateTime64 precision tz)
  where
  chTypeName =
    let
      prec = show (natVal @precision Proxy)
    in
    case symbolVal @tz Proxy of
      "" -> "DateTime64(" <> prec <> ")"
      tz -> "DateTime64(" <> prec <> ", '" <> tz <> "')"
  defaultValueOfTypeName = MkDateTime64 0

instance Serializable (DateTime64 precision tz) where
  serialize _ (MkDateTime64 w64) = word64LE w64
  deserialize _ = MkDateTime64 <$> getWord64le
  {-# INLINE deserialize #-}

instance ToChType (DateTime64 precision tz) Word64 where
  toChType = MkDateTime64
  fromChType (MkDateTime64 w64) = w64

instance KnownNat precision => ToChType (DateTime64 precision tz) UTCTime where
  toChType = MkDateTime64 . floor . (* (10 ^ natVal (Proxy @precision)))
    . utcTimeToPOSIXSeconds
  fromChType (MkDateTime64 w64) = posixSecondsToUTCTime
    $ (/ (10 ^ natVal (Proxy @precision))) $ fromIntegral w64

-- ToDo: Need to be fixed
-- instance ToQueryPart (DateTime64 precision tz)
--   where
--   toQueryPart chDateTime =
--     let time = BS8.pack . show . fromChType @_ @Word64 $ chDateTime
--     in byteString (BS8.replicate (12 - BS8.length time) '0' <> time)


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


-- ** LowCardinality

-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
instance IsLowCardinalitySupported chType => IsChType (LowCardinality chType)
  where
  chTypeName = "LowCardinality(" <> chTypeName @chType <> ")"
  defaultValueOfTypeName = MkLowCardinality $ defaultValueOfTypeName @chType

deriving newtype instance (Eq chType, IsLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving newtype instance (NFData chType, IsLowCardinalitySupported chType) => NFData (LowCardinality chType)
deriving newtype instance IsString (LowCardinality ChString)

class IsChType chType => IsLowCardinalitySupported chType
instance IsLowCardinalitySupported ChString
instance
  ( IsLowCardinalitySupported chType
  , IsChType (Nullable chType)
  ) =>
  IsLowCardinalitySupported (Nullable chType)

instance {-# OVERLAPPABLE #-}
  ( IsChType chType
  , TypeError
    (    'Text "LowCardinality("  ':<>: 'ShowType chType  ':<>: 'Text ") is unsupported"
    ':$$: 'Text "Use one of these types:"
    ':$$: 'Text "  ChString"
    ':$$: 'Text "  DateTime"
    ':$$: 'Text "  Nullable(T)"
    )
  ) => IsLowCardinalitySupported chType

instance
  ToChType inputType chType
  =>
  ToChType (LowCardinality inputType) chType where
  toChType = MkLowCardinality . toChType
  fromChType (MkLowCardinality lc)= fromChType @inputType lc

instance ToQueryPart chType => ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType


-- ** Generics

class GSerial f where
  gSerialize :: ProtocolRevision -> f p -> Builder
  gDeserialize :: ProtocolRevision -> Get (f p)

instance GSerial f => GSerial (D1 c (C1 c2 f)) where
  gSerialize rev (M1 (M1 re)) = gSerialize rev re
  {-# INLINE gSerialize #-}
  gDeserialize rev = M1 . M1 <$> gDeserialize rev
  {-# INLINE gDeserialize #-}

instance (GSerial left1,  GSerial right) => GSerial (left1 :*: right) where
  gSerialize rev (l :*: r) = gSerialize rev l <> gSerialize rev r
  {-# INLINE gSerialize #-}
  gDeserialize rev = do
    liftA2 (:*:)
      (gDeserialize rev)
      (gDeserialize rev)
  {-# INLINE gDeserialize #-}

instance
  (Serializable chType, Not (sel == "server_revision") ~ True)
  =>
  GSerial (S1 ('MetaSel ('Just sel) a b c) (Rec0 chType)) where
  gSerialize rev (M1 (K1 re)) = serialize rev re
  {-# INLINE gSerialize #-}
  gDeserialize rev = M1 . K1 <$> deserialize @chType rev
  {-# INLINE gDeserialize #-}


instance {-# OVERLAPPING #-}
  GSerial right
  =>
  GSerial (S1 ('MetaSel ('Just "server_revision") a b c) (Rec0 ProtocolRevision) :*: right)
  where
  gSerialize rev (M1 (K1 (MkProtocolRevision server_rev)) :*: right)= do
    serialize rev server_rev <> gSerialize rev right
  {-# INLINE gSerialize #-}
  gDeserialize rev = do
    chosenRev <- min rev . MkProtocolRevision <$> deserialize @UVarInt rev
    liftA2 (:*:)
      (pure . M1 . K1 $ chosenRev)
      (gDeserialize @right chosenRev)
  {-# INLINE gDeserialize #-}




-- * Protocol parts

-- ** UVarInt

{- |
  Unsigned variable-length quantity encoding

  Part of protocol implementation
-}
newtype UVarInt = MkUVarInt Word64
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance Serializable UVarInt where
  serialize _ = goUVarIntSer
    where
    goUVarIntSer i
      | i < 0x80 = word8 (fromIntegral i)
      | otherwise = word8 (setBit (fromIntegral i) 7) <> goUVarIntSer (i `unsafeShiftR` 7)
  deserialize _ = goUVarIntDeser 0 (0 :: UVarInt)
    where
    goUVarIntDeser i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else goUVarIntDeser (i + 1) $! o'
    goUVarIntDeser _ _ = fail "input exceeds varuint size"
  {-# INLINE deserialize #-}


-- ** Versioning

major, minor, patch :: UVarInt
major = case versionBranch version of (x:_) -> fromIntegral x; _ -> 0
minor = case versionBranch version of (_:x:_) -> fromIntegral x; _ -> 0
patch = case versionBranch version of (_:_:x:_) -> fromIntegral x; _ -> 0

clientName :: ChString
clientName = "ClickHaskell"

newtype ProtocolRevision = MkProtocolRevision UVarInt
  deriving newtype (Eq, Num, Ord, Serializable)

mkRev :: forall nat . KnownNat nat => ProtocolRevision
mkRev = (fromIntegral . natVal) (Proxy @nat)

{- |
Protocol implementation part for backward compatilibity formalization

NB:

  Be carefull with `BeforeRevision` value.

  If **chosen protocol** revision would be >= **revisionNumber**
  then you would get an exception during serialization.

  To avoid this:

  1. On client side - provide `AfterRevision` with some empty value
  2. On proxy side - provide server-to-server packets mapping with fallbacks on revision upgrade

-}
data Revisioned (revisionNumber :: Nat) b a = BeforeRevision b | AfterRevision a

type SinceRevision after rev = Revisioned rev () after

instance
  (KnownNat revision, Serializable before, Serializable after)
  =>
  Serializable (Revisioned revision before after)
  where
  serialize rev sinceRevVal =
    if rev < mkRev @revision
    then mempty
    else case sinceRevVal of
      BeforeRevision _b -> error "Protocol-specific implementation error" -- Watch `Revisioned` note
      AfterRevision a -> serialize rev a
  deserialize rev =
    if rev < mkRev @revision
    then BeforeRevision <$> deserialize @before rev
    else AfterRevision <$> deserialize @after rev

instance Serializable () where
  serialize _ () = ""
  deserialize _ = pure ()


{-
  Slightly modified C++ sources:
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Core/ProtocolDefines.h#L6
-}
type DBMS_TCP_PROTOCOL_VERSION = DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS;

type DBMS_MIN_REVISION_WITH_CLIENT_INFO = 54032;
type DBMS_MIN_REVISION_WITH_SERVER_TIMEZONE = 54058;
type DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO = 54060;
type DBMS_MIN_REVISION_WITH_TABLES_STATUS = 54226;
type DBMS_MIN_REVISION_WITH_TIME_ZONE_PARAMETER_IN_DATETIME_DATA_TYPE = 54337;
type DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME = 54372;
type DBMS_MIN_REVISION_WITH_VERSION_PATCH = 54401;
type DBMS_MIN_REVISION_WITH_SERVER_LOGS = 54406;
type DBMS_MIN_REVISION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 54448;
type DBMS_MIN_MAJOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 21;
type DBMS_MIN_MINOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 4;
type DBMS_MIN_REVISION_WITH_COLUMN_DEFAULTS_METADATA = 54410;
type DBMS_MIN_REVISION_WITH_LOW_CARDINALITY_TYPE = 54405;
type DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO = 54420;
type DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS = 54429;
type DBMS_MIN_REVISION_WITH_SCALARS = 54429;
type DBMS_MIN_REVISION_WITH_OPENTELEMETRY = 54442;
type DBMS_MIN_REVISION_WITH_AGGREGATE_FUNCTIONS_VERSIONING = 54452;
type DBMS_CLUSTER_INITIAL_PROCESSING_PROTOCOL_VERSION = 1;
type DBMS_CLUSTER_PROCESSING_PROTOCOL_VERSION_WITH_DATA_LAKE_METADATA = 2;
type DBMS_CLUSTER_PROCESSING_PROTOCOL_VERSION = 2;
type DATA_LAKE_TABLE_STATE_SNAPSHOT_PROTOCOL_VERSION = 1;
type DBMS_MIN_SUPPORTED_PARALLEL_REPLICAS_PROTOCOL_VERSION = 3;
type DBMS_PARALLEL_REPLICAS_MIN_VERSION_WITH_MARK_SEGMENT_SIZE_FIELD = 4;
type DBMS_PARALLEL_REPLICAS_MIN_VERSION_WITH_PROJECTION = 5;
type DBMS_PARALLEL_REPLICAS_PROTOCOL_VERSION = 5;
type DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS = 54453;
type DBMS_MIN_REVISION_WITH_QUERY_AND_LINE_NUMBERS = 54475;
type DBMS_MERGE_TREE_PART_INFO_VERSION = 1;
type DBMS_QUERY_PLAN_SERIALIZATION_VERSION = 0;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET = 54441;
type DBMS_MIN_REVISION_WITH_X_FORWARDED_FOR_IN_CLIENT_INFO = 54443;
type DBMS_MIN_REVISION_WITH_REFERER_IN_CLIENT_INFO = 54447;
type DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH = 54448;
type DBMS_MIN_PROTOCOL_VERSION_WITH_INCREMENTAL_PROFILE_EVENTS = 54451;
type DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION = 54454;
type DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME = 54449;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PROFILE_EVENTS_IN_INSERT = 54456;
type DBMS_MIN_PROTOCOL_VERSION_WITH_VIEW_IF_PERMITTED = 54457;
type DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS = 54459;
type DBMS_MIN_PROTOCOL_VERSION_WITH_SERVER_QUERY_TIME_IN_PROGRESS = 54460;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES = 54461;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2 = 54462;
type DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS = 54463;
type DBMS_MIN_PROTOCOL_VERSION_WITH_TIMEZONE_UPDATES = 54464;
type DBMS_MIN_REVISION_WITH_SPARSE_SERIALIZATION = 54465; -- ToDo
type DBMS_MIN_REVISION_WITH_SSH_AUTHENTICATION = 54466;
type DBMS_MIN_REVISION_WITH_TABLE_READ_ONLY_CHECK = 54467;
type DBMS_MIN_REVISION_WITH_SYSTEM_KEYWORDS_TABLE = 54468;
type DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION = 54469;
type DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS = 54470;
type DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL = 54471;
type DBMS_MIN_PROTOCOL_VERSION_WITH_INTERSERVER_EXTERNALLY_GRANTED_ROLES = 54472;
type DBMS_MIN_REVISION_WITH_V2_DYNAMIC_AND_JSON_SERIALIZATION = 54473;
type DBMS_MIN_REVISION_WITH_SERVER_SETTINGS = 54474;
type DBMS_MIN_REVISON_WITH_JWT_IN_INTERSERVER = 54476;
type DBMS_MIN_REVISION_WITH_QUERY_PLAN_SERIALIZATION = 54477;
type DBMS_MIN_REVISON_WITH_PARALLEL_BLOCK_MARSHALLING = 54478;
type DBMS_MIN_REVISION_WITH_VERSIONED_CLUSTER_FUNCTION_PROTOCOL = 54479;
type DBMS_MIN_REVISION_WITH_OUT_OF_ORDER_BUCKETS_IN_AGGREGATION = 54480;
type DBMS_MIN_REVISION_WITH_COMPRESSED_LOGS_PROFILE_EVENTS_COLUMNS = 54481;
