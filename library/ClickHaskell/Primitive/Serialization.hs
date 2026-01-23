module ClickHaskell.Primitive.Serialization where

-- GHC included
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Binary.Get
import Data.Bits (Bits (setBit, unsafeShiftL, unsafeShiftR, (.&.), (.|.)), xor)
import Data.ByteString.Builder
import Data.Int (Int64)
import Data.Type.Bool (Not)
import Data.Type.Equality (type (==))
import Data.Typeable (Proxy (..))
import Data.Word (Word64)
import GHC.Generics (C1, D1, Generic (..), K1 (K1), M1 (M1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Prelude hiding (liftA2)


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

instance Serializable () where
  serialize _ () = ""
  deserialize _ = pure ()


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

-- * Generics


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

newtype VarInt = MkVarInt Int64
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance Serializable VarInt where
  serialize rev (MkVarInt int64) =
    serialize rev (MkUVarInt (zigZagEncode int64))
    where
    zigZagEncode :: Int64 -> Word64
    zigZagEncode i = fromIntegral ((i `unsafeShiftL` 1) `xor` (i `unsafeShiftR` 63))
    {-# INLINE zigZagEncode #-}

  {-# INLINE deserialize #-}
  deserialize rev = do
    MkUVarInt u <- deserialize rev
    pure $! MkVarInt (zigZagDecode u)
    where
    zigZagDecode :: Word64 -> Int64
    zigZagDecode u =
      fromIntegral ((u `unsafeShiftR` 1) `xor` negate (u .&. 1))
    {-# INLINE zigZagDecode #-}

-- >>> show major <> "." <> show minor <> "." <> show patch
-- "1.0.0"
major, minor, patch :: UVarInt
major = 1
minor = 1
patch = 0

newtype ProtocolRevision = MkProtocolRevision UVarInt
  deriving newtype (Eq, Num, Ord, Serializable)

mkRev :: forall nat . KnownNat nat => ProtocolRevision
mkRev = (fromIntegral . natVal) (Proxy @nat)


-- * Protocol parts

-- ** Versioning

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
