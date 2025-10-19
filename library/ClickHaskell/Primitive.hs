{-# LANGUAGE BangPatterns #-}
module ClickHaskell.Primitive where

-- Internal
import Paths_ClickHaskell (version)

-- GHC included
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Binary.Get
import Data.Bits (Bits (setBit, unsafeShiftL, unsafeShiftR, (.&.), (.|.)))
import Data.ByteString as BS (ByteString, length)
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int16, Int32, Int64, Int8)
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


-- ** Int16

instance IsChType Int16 where
  chTypeName = "Int16"
  defaultValueOfTypeName = 0

instance Serializable Int16 where
  serialize _ = int16LE
  deserialize _ = getInt16le
  {-# INLINE deserialize #-}


-- ** Int32

instance IsChType Int32 where
  chTypeName = "Int32"
  defaultValueOfTypeName = 0

instance Serializable Int32 where
  serialize _ = int32LE
  deserialize _ = getInt32le
  {-# INLINE deserialize #-}


-- ** Int64

instance IsChType Int64 where
  chTypeName = "Int64"
  defaultValueOfTypeName = 0

instance Serializable Int64 where
  serialize _ = int64LE
  deserialize _ = getInt64le
  {-# INLINE deserialize #-}


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


-- ** DateTime

{- |
ClickHouse DateTime column type (paramtrized with timezone)

>>> chTypeName @(DateTime "")
"DateTime"
>>> chTypeName @(DateTime "UTC")
"DateTime('UTC')"
-}
newtype DateTime (tz :: Symbol) = MkDateTime Word32
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance KnownSymbol tz => IsChType (DateTime tz)
  where
  chTypeName = case (symbolVal @tz Proxy) of
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


-- ** DateTime64

{- |
ClickHouse DateTime64 column type (paramtrized with timezone)

>>> chTypeName @(DateTime64 3 "")
"DateTime64(3)"
>>> chTypeName @(DateTime64 3 "UTC")
"DateTime64(3, 'UTC')"
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
clientName = fromString $
  "ClickHaskell-" <> show major <> "." <> show minor <> "." <> show patch

newtype ProtocolRevision = MkProtocolRevision UVarInt
  deriving newtype (Eq, Num, Ord, Serializable)

{-# INLINE [0] afterRevision #-}
afterRevision
  :: forall rev monoid
  . (KnownNat rev, Monoid monoid)
  => ProtocolRevision -> monoid -> monoid
afterRevision chosenRevision monoid =
  if chosenRevision >= (fromIntegral . natVal) (Proxy @rev)
  then monoid
  else mempty

latestSupportedRevision :: ProtocolRevision
latestSupportedRevision = mkRev @DBMS_TCP_PROTOCOL_VERSION

mkRev :: forall nat . KnownNat nat => ProtocolRevision
mkRev = (fromIntegral . natVal) (Proxy @nat)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented

instance
  (KnownNat revision, Serializable chType)
  =>
  Serializable (SinceRevision chType revision)
  where
  serialize rev (MkSinceRevision val) = afterRevision @revision rev (serialize rev val)
  serialize rev NotPresented          = afterRevision @revision rev (error "Unexpected error")

  deserialize rev =
    if rev >= mkRev @revision
    then MkSinceRevision <$> deserialize @chType rev
    else pure NotPresented


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
type DBMS_MIN_REVISION_WITH_SPARSE_SERIALIZATION = 54465;
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
