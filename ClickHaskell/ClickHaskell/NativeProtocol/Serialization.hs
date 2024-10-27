{-# LANGUAGE 
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TemplateHaskell
  , UndecidableInstances
#-}

module ClickHaskell.NativeProtocol.Serialization where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol.Columns
import Paths_ClickHaskell (version)

-- GHC included
import Control.Monad (replicateM)
import Data.Binary (Binary (..))
import Data.Binary.Get
import Data.Binary.Get.Internal (readN)
import Data.Binary.Put (execPut)
import Data.ByteString as BS (StrictByteString, length, take, toStrict)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Builder as BS (byteString, word64LE)
import Data.Typeable (Proxy (..))
import Data.Version (Version (..), showVersion)
import Data.WideWord (Int128 (..), Word128 (..))
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (lift)

-- * Serializable

class Serializable chType
  where
  default serialize :: (Generic chType, GSerializable (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from


instance Serializable chType => Serializable [chType] where
  serialize rev list =
    serialize @UVarInt rev (fromIntegral $ Prelude.length list)
    <> mconcat (map (serialize @chType rev) list)

instance Serializable UVarInt where serialize _ = execPut . put
instance Serializable ChUInt8 where serialize _ = execPut . put
instance Serializable ChUInt16 where serialize _ = execPut . put
instance Serializable ChUInt32 where serialize _ = execPut . put
instance Serializable ChUInt64 where serialize _ = execPut . put
instance Serializable ChUInt128 where serialize _ = (\(Word128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChInt8 where serialize _ = execPut . put
instance Serializable ChInt16 where serialize _ = execPut . put
instance Serializable ChInt32 where serialize _ = execPut . put
instance Serializable ChInt64 where serialize _ = execPut . put
instance Serializable ChInt128 where serialize _ = (\(Int128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChString where
  serialize revision str
    =  (serialize @UVarInt revision . fromIntegral . BS.length . fromChType) str
    <> (BS.byteString . fromChType @_ @StrictByteString) str

-- No columns special case
instance Serializable (Columns '[]) where
  serialize _ _ = ""

instance
  ( Serializable (Column name1 chType1)
  , Serializable (Columns (one ': xs))
  )
  =>
  Serializable (Columns (Column name1 chType1 ': one ': xs)) where
  serialize rev (AddColumn column extraColumns) = serialize rev column <> serialize rev extraColumns

instance
  Serializable (Column name chType)
  =>
  Serializable (Columns '[Column name chType])
  where
  serialize rev (AddColumn column Empty) = serialize rev column

instance
  ( CompiledColumn (Column name chType)
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name chType) where
  serialize rev (MkColumn values)
    =  serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) values)


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

instance Deserializable chType => Deserializable [chType] where
  deserialize rev = do
    len <- deserialize @UVarInt rev
    replicateM (fromIntegral len) (deserialize @chType rev)

instance Deserializable UVarInt where deserialize _ = get
instance Deserializable ChUInt8 where deserialize _ = get
instance Deserializable ChUInt16 where deserialize _ = get
instance Deserializable ChUInt32 where deserialize _ = get
instance Deserializable ChUInt64 where deserialize _ = get
instance Deserializable ChUInt128 where deserialize _ = toChType <$> (Word128 <$> getWord64le <*> getWord64le)
instance Deserializable ChInt8 where deserialize _ = get
instance Deserializable ChInt16 where deserialize _ = get
instance Deserializable ChInt32 where deserialize _ = get
instance Deserializable ChInt64 where deserialize _ = get
instance Deserializable ChInt128 where deserialize _ = toChType <$> (Int128 <$> getWord64le <*> getWord64le)
instance Deserializable ChString where
  deserialize revision = do
    strSize <- fromIntegral <$> deserialize @UVarInt revision
    toChType <$> readN strSize (BS.take strSize)

-- No columns special case
instance Deserializable (Columns '[]) where
  deserialize _ = pure emptyColumns

instance
  ( Deserializable (Column name1 chType1)
  , Deserializable (Columns (one ': xs))
  )
  =>
  Deserializable (Columns (Column name1 chType1 ': one ': xs)) where
  deserialize rev = do
    a <- deserialize rev
    b <- deserialize rev
    pure $ appendColumn a b


instance
  Deserializable (Column name chType)
  =>
  Deserializable (Columns '[Column name chType])
  where
  deserialize rev = (`appendColumn` emptyColumns) <$> deserialize rev

instance
  ( CompiledColumn (Column name chType)
  , IsChType chType
  , Deserializable chType
  ) => Deserializable (Column name chType) where
  deserialize _rev = undefined


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
  Deserializable chType
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  gDeserialize rev =  M1 . K1 <$> deserialize @chType rev

instance
  (Deserializable chType, GDeserializable right)
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType) :*: right)
  where
  gDeserialize rev = do
    chType <- deserialize @chType rev
    right <- gDeserialize rev
    pure ((M1 . K1) chType :*: right)

instance {-# OVERLAPS #-}
  GDeserializable right
  =>
  GDeserializable (S1 (MetaSel (Just "server_revision") a b f) (Rec0 UVarInt) :*: right)
  where
  gDeserialize client_revision = do
    server_revision <- deserialize @UVarInt client_revision
    right <- gDeserialize @right (min server_revision client_revision)
    pure ((M1 . K1) server_revision :*: right)




-- * Versioning

-- ** Protocol compatibility wrappers

type ProtocolRevision = UVarInt

afterRevision
  :: forall (revision :: Nat) monoid
  .  (KnownNat revision, Monoid monoid)
  => ProtocolRevision -> monoid -> monoid
afterRevision chosenRevision monoid =
  if chosenRevision >= (fromIntegral . natVal) (Proxy @revision)
  then monoid
  else mempty

latestSupportedRevision :: ProtocolRevision
latestSupportedRevision = (fromIntegral . natVal) (Proxy @DBMS_TCP_PROTOCOL_VERSION)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented

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


-- ** Client versioning

clientMajorVersion, clientMinorVersion, clientPatchVersion :: UVarInt
clientMajorVersion = fromIntegral $(lift (versionBranch version !! 0))
clientMinorVersion = fromIntegral $(lift (versionBranch version !! 1))
clientPatchVersion = fromIntegral $(lift (versionBranch version !! 2))

clientNameAndVersion :: ChString
clientNameAndVersion = $(lift ("ClickHaskell-" <> showVersion version))


-- ** Versions

{-
  Slightly modified C++ sources:
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Core/ProtocolDefines.h#L6
-}
type DBMS_TCP_PROTOCOL_VERSION = 54471;

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
type DBMS_CLUSTER_PROCESSING_PROTOCOL_VERSION = 1;
type DBMS_MIN_SUPPORTED_PARALLEL_REPLICAS_PROTOCOL_VERSION = 3;
type DBMS_PARALLEL_REPLICAS_MIN_VERSION_WITH_MARK_SEGMENT_SIZE_FIELD = 4;
type DBMS_PARALLEL_REPLICAS_PROTOCOL_VERSION = 4;
type DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS = 54453;
type DBMS_MERGE_TREE_PART_INFO_VERSION = 1;
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
