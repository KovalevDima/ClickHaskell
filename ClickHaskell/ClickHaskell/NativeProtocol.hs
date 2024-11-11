{-# LANGUAGE
    DuplicateRecordFields
  , OverloadedStrings
  , RecordWildCards
  , TemplateHaskell
#-}

module ClickHaskell.NativeProtocol where

-- Internal dependencies
import ClickHaskell.DbTypes
import Paths_ClickHaskell (version)

-- GHC included
import Control.Monad (replicateM)
import Data.Text
import Data.Typeable (Proxy (..))
import Data.Version (Version (..), showVersion)
import Language.Haskell.TH.Syntax (lift)
import GHC.Generics
import GHC.TypeLits

-- * Client packets

data ClientPacketType
  = Hello
  | Query
  | Data
  | Cancel
  | Ping
  | TablesStatusRequest
  | KeepAlive
  | Scalar
  | IgnoredPartUUIDs
  | ReadTaskResponse
  | MergeTreeReadTaskResponse
  | SSHChallengeRequest
  | SSHChallengeResponse
  deriving (Enum, Show)

type family PacketTypeNumber (packetType :: ClientPacketType)
  where
  PacketTypeNumber Hello = 0
  PacketTypeNumber Query = 1
  PacketTypeNumber Data = 2
  PacketTypeNumber Cancel = 3
  PacketTypeNumber Ping = 4
  PacketTypeNumber TablesStatusRequest = 5
  PacketTypeNumber KeepAlive = 6
  PacketTypeNumber Scalar = 7
  PacketTypeNumber IgnoredPartUUIDs = 8
  PacketTypeNumber ReadTaskResponse = 9
  PacketTypeNumber MergeTreeReadTaskResponse = 10
  PacketTypeNumber SSHChallengeRequest = 11
  PacketTypeNumber SSHChallengeResponse = 12

data Packet (packetType :: ClientPacketType) = MkPacket
instance KnownNat (PacketTypeNumber packetType) => Show (Packet (packetType :: ClientPacketType)) where
  show _ = show . toEnum @ClientPacketType . fromIntegral $ packetNumVal @packetType

packetNumVal :: forall packetType . KnownNat (PacketTypeNumber packetType) => UVarInt
packetNumVal = fromIntegral . natVal $ Proxy @(PacketTypeNumber packetType)

instance
  KnownNat (PacketTypeNumber packetType)
  =>
  Serializable (Packet (packetType :: ClientPacketType)) where
  serialize rev _ = serialize @UVarInt rev (packetNumVal @packetType)

instance Deserializable (Packet (packetType :: ClientPacketType)) where
  deserialize _rev = pure (MkPacket @packetType)


-- ** Hello

data HelloParameters = MkHelloParameters
  { chDatabase :: Text
  , chLogin :: Text
  , chPass :: Text
  }

mkHelloPacket :: HelloParameters -> HelloPacket
mkHelloPacket MkHelloParameters{chDatabase, chLogin, chPass} =
  MkHelloPacket
    { packet_type          = MkPacket
    , client_name          = clientNameAndVersion
    , client_version_major = clientMajorVersion
    , client_version_minor = clientMinorVersion
    , tcp_protocol_version = latestSupportedRevision
    , default_database     = toChType chDatabase
    , user                 = toChType chLogin
    , password             = toChType chPass
    }

data HelloPacket = MkHelloPacket
  { packet_type          :: Packet Hello
  , client_name          :: ChString
  , client_version_major :: UVarInt
  , client_version_minor :: UVarInt
  , tcp_protocol_version :: UVarInt
  , default_database     :: ChString
  , user                 :: ChString
  , password             :: ChString
  }
  deriving (Generic, Serializable)


mkAddendum :: Addendum
mkAddendum = MkAddendum
  { quota_key = MkSinceRevision ""
  }

data Addendum = MkAddendum
  { quota_key :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY
  }
  deriving (Generic, Serializable)


-- ** Ping

mkPingPacket :: PingPacket
mkPingPacket = MkPingPacket{packet_type = MkPacket}

data PingPacket = MkPingPacket{packet_type :: Packet Ping}
  deriving (Generic, Serializable)


-- ** Query

mkQueryPacket :: ProtocolRevision -> ChString -> ChString -> QueryPacket
mkQueryPacket chosenRev user query = MkQueryPacket
  { query_packet = MkPacket
  , query_id = ""
  , client_info                    = MkSinceRevision MkClientInfo
    { query_kind                   = InitialQuery
    , initial_user                 = user
    , initial_query_id             = ""
    , initial_adress               = "0.0.0.0:0"
    , initial_time                 = MkSinceRevision 0
    , interface_type               = 1 -- [tcp - 1, http - 2]
    , os_user                      = "dmitry"
    , hostname                     = "desktop"
    , client_name                  = clientNameAndVersion
    , client_major                 = clientMajorVersion
    , client_minor                 = clientMinorVersion
    , client_revision              = chosenRev
    , quota_key                    = MkSinceRevision ""
    , distrubuted_depth            = MkSinceRevision 0
    , client_patch                 = MkSinceRevision clientPatchVersion
    , open_telemetry               = MkSinceRevision 0
    , collaborate_with_initiator   = MkSinceRevision 0
    , count_participating_replicas = MkSinceRevision 0
    , number_of_current_replica    = MkSinceRevision 0
    }
  , settings           = MkDbSettings
  , interserver_secret = MkSinceRevision ""
  , query_stage        = Complete
  , compression        = 0
  , query              = query
  , parameters         = MkSinceRevision MkQueryParameters
  }

data QueryPacket = MkQueryPacket
  { query_packet       :: Packet Query
  , query_id           :: ChString
  , client_info        :: ClientInfo `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_INFO
  , settings           :: DbSettings
  , interserver_secret :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
  , query_stage        :: QueryStage
  , compression        :: UVarInt
  , query              :: ChString
  , parameters         :: QueryParameters `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
  }
  deriving (Generic, Serializable)

data DbSettings = MkDbSettings
instance Serializable DbSettings where serialize rev _ = serialize @ChString rev ""

data QueryParameters = MkQueryParameters
instance Serializable QueryParameters where serialize rev _ = serialize @ChString rev ""

data QueryStage
  = FetchColumns
  | WithMergeableState
  | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

instance Serializable QueryStage where
  serialize rev = serialize @UVarInt rev . fromIntegral . fromEnum

queryStageCode :: QueryStage -> UVarInt
queryStageCode = fromIntegral . fromEnum

data Flags = IMPORTANT | CUSTOM | OBSOLETE
flagCode :: Flags -> ChUInt64
flagCode IMPORTANT = 0x01
flagCode CUSTOM    = 0x02
flagCode OBSOLETE  = 0x04

data ClientInfo = MkClientInfo
  { query_kind                   :: QueryKind
  , initial_user                 :: ChString
  , initial_query_id             :: ChString
  , initial_adress               :: ChString
  , initial_time                 :: ChInt64 `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME
  , interface_type               :: ChUInt8
  , os_user                      :: ChString
  , hostname                     :: ChString
  , client_name                  :: ChString
  , client_major                 :: UVarInt
  , client_minor                 :: UVarInt
  , client_revision              :: UVarInt
  , quota_key                    :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
  , distrubuted_depth            :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
  , client_patch                 :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , open_telemetry               :: ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_OPENTELEMETRY
  , collaborate_with_initiator   :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , count_participating_replicas :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , number_of_current_replica    :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  }
  deriving (Generic, Serializable)

data QueryKind = NoQuery | InitialQuery | SecondaryQuery
  deriving (Enum)

instance Serializable QueryKind where
  serialize rev = serialize @ChUInt8 rev . fromIntegral . fromEnum


-- ** Data

mkDataPacket :: ChString -> UVarInt -> UVarInt -> DataPacket
mkDataPacket table_name columns_count rows_count =
  MkDataPacket
    { packet_type   = MkPacket
    , table_name
    , block_info    = MkBlockInfo
      { field_num1   = 1, is_overflows = 0
      , field_num2   = 2, bucket_num   = -1
      , eof          = 0
      }
    , columns_count
    , rows_count
    }

data DataPacket = MkDataPacket
  { packet_type   :: Packet Data
  , table_name    :: ChString
  , block_info    :: BlockInfo
  , columns_count :: UVarInt
  , rows_count    :: UVarInt
  }
  deriving (Generic, Serializable, Deserializable, Show)

data BlockInfo = MkBlockInfo
  { field_num1   :: UVarInt, is_overflows :: ChUInt8
  , field_num2   :: UVarInt, bucket_num   :: ChInt32
  , eof          :: UVarInt
  }
  deriving (Generic, Serializable, Deserializable, Show)




-- * Server packets

data ServerPacketType where
  HelloResponse :: HelloResponse -> ServerPacketType
  DataResponse :: DataPacket -> ServerPacketType
  Exception :: ExceptionPacket -> ServerPacketType
  Progress :: ProgressPacket -> ServerPacketType
  Pong :: ServerPacketType
  EndOfStream :: ServerPacketType
  ProfileInfo :: ProfileInfo -> ServerPacketType
  Totals :: ServerPacketType
  Extremes :: ServerPacketType
  TablesStatusResponse :: ServerPacketType
  Log :: ServerPacketType
  TableColumns :: ServerPacketType
  UUIDs :: ServerPacketType
  ReadTaskRequest :: ServerPacketType
  ProfileEvents :: ServerPacketType
  UnknownPacket :: UVarInt -> ServerPacketType

instance Deserializable ServerPacketType where
  deserialize rev = do
    packetNum <- deserialize @UVarInt rev
    case packetNum of
      0  -> HelloResponse <$> deserialize rev
      1  -> DataResponse <$> deserialize rev
      2  -> Exception <$> deserialize rev
      3  -> Progress <$> deserialize rev
      4  -> pure Pong
      5  -> pure EndOfStream
      6  -> ProfileInfo <$> deserialize rev
      7  -> pure Totals
      8  -> pure Extremes
      9  -> pure TablesStatusResponse
      10 -> pure Log
      11 -> pure TableColumns
      12 -> pure UUIDs
      13 -> pure ReadTaskRequest
      14 -> pure ProfileEvents
      _  -> pure $ UnknownPacket packetNum

instance Show ServerPacketType where
  show (HelloResponse hello) = "HelloResponse " <> show hello
  show (DataResponse dataPacket) = "DataResponse" <> show dataPacket
  show (Exception exception) = "Exception" <> show exception
  show (Progress progress) = "Progress" <> show progress
  show Pong = "Pong"
  show EndOfStream = "EndOfStream"
  show (ProfileInfo profileInfo) = "ProfileInfo" <> show profileInfo 
  show Totals = "Totals"
  show Extremes = "Extremes"
  show TablesStatusResponse = "TablesStatusResponse"
  show Log = "Log"
  show TableColumns = "TableColumns"
  show UUIDs = "UUIDs"
  show ReadTaskRequest = "ReadTaskRequest"
  show ProfileEvents = "ProfileEvents"
  show (UnknownPacket packetNum) = "UnknownPacket: " <> show packetNum

-- ** HelloResponse

{-
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Client/Connection.cpp#L520
-}
data HelloResponse = MkHelloResponse
  { server_name                    :: ChString
  , server_version_major           :: UVarInt
  , server_version_minor           :: UVarInt
  , server_revision                :: UVarInt
  , server_parallel_replicas_proto :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL
  , server_timezone                :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_SERVER_TIMEZONE
  , server_display_name            :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME
  , server_version_patch           :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , proto_send_chunked_srv         :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , proto_recv_chunked_srv         :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , password_complexity_rules      :: [PasswordComplexityRules] `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES
  , read_nonce                     :: ChUInt64 `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2
  }
  deriving (Generic, Show)

instance Deserializable HelloResponse where
  deserialize revision = do
    server_name                    <- deserialize revision
    server_version_major           <- deserialize revision
    server_version_minor           <- deserialize revision
    server_revision                <- deserialize revision
    -- Override current protocol revision for backward compatibility
    let chosenRevision = min server_revision revision
    server_parallel_replicas_proto <- deserialize chosenRevision
    server_timezone                <- deserialize chosenRevision
    server_display_name            <- deserialize chosenRevision
    server_version_patch           <- deserialize chosenRevision
    proto_send_chunked_srv         <- deserialize chosenRevision
    proto_recv_chunked_srv         <- deserialize chosenRevision
    password_complexity_rules      <- deserialize chosenRevision
    read_nonce                     <- deserialize chosenRevision
    pure MkHelloResponse{..}

data PasswordComplexityRules = MkPasswordComplexityRules
  { original_pattern  :: ChString
  , exception_message :: ChString
  }
  deriving (Generic, Deserializable, Show)

instance Deserializable [PasswordComplexityRules] where
  deserialize rev = do
    len <- deserialize @UVarInt rev
    replicateM (fromIntegral len) (deserialize @PasswordComplexityRules rev)

-- ** Exception

data ExceptionPacket = MkExceptionPacket
  { code        :: ChInt32
  , name        :: ChString
  , message     :: ChString
  , stack_trace :: ChString
  , nested      :: ChUInt8
  }
  deriving (Generic, Deserializable, Show)

-- ** Progress

data ProgressPacket = MkProgressPacket
  { rows        :: UVarInt
  , bytes       :: UVarInt
  , total_rows  :: UVarInt
  , total_bytes :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_rows  :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_bytes :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  , elapsed_ns  :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  }
  deriving (Generic, Deserializable, Show)

-- ** ProfileInfo

data ProfileInfo = MkProfileInfo
  { rows                         :: UVarInt
  , blocks                       :: UVarInt
  , bytes                        :: UVarInt
  , applied_limit                :: ChUInt8
  , rows_before_limit            :: UVarInt
  , calculated_rows_before_limit :: ChUInt8
  , applied_aggregation          :: ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  , rows_before_aggregation      :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  }
  deriving (Generic, Deserializable, Show)




-- * Versioning

-- ** Protocol compatibility wrappers

{-# INLINE [0] afterRevision #-}
afterRevision
  :: forall (revision :: Nat) monoid
  .  (KnownNat revision, Monoid monoid)
  => ProtocolRevision -> monoid -> monoid
afterRevision chosenRevision monoid =
  if chosenRevision >= (fromIntegral . natVal) (Proxy @revision)
  then monoid
  else mempty

{-# INLINE [0] latestSupportedRevision #-}
latestSupportedRevision :: ProtocolRevision
latestSupportedRevision = (fromIntegral . natVal) (Proxy @DBMS_TCP_PROTOCOL_VERSION)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented
instance Show a => Show (SinceRevision a (revisionNumber :: Nat)) where
  show (MkSinceRevision a) = show a
  show NotPresented = ""

instance
  ( KnownNat revision
  , Serializable chType
  )
  =>
  Serializable (SinceRevision chType revision)
  where
  serialize rev (MkSinceRevision val) = afterRevision @revision rev (serialize rev val)
  serialize rev NotPresented          = afterRevision @revision rev (error "Unexpected error")

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
type DBMS_TCP_PROTOCOL_VERSION = 54448;

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
