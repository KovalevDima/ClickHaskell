module ClickHaskell.Packets where

-- Internal
import ClickHaskell.Primitive

-- GHC
import Data.ByteString.Builder (Builder)
import Data.Int
import GHC.Generics

-- * Common Data packet

data DataPacket = MkDataPacket
  { table_name    :: ChString
  , block_info    :: BlockInfo
  , columns_count :: UVarInt
  , rows_count    :: UVarInt
  }
  deriving (Generic, Serializable)

serializeDataPacket :: ChString -> UVarInt -> UVarInt -> (ProtocolRevision -> Builder)
serializeDataPacket table_name columns_count rows_count =
  flip serialize $ Data
    MkDataPacket
      { table_name
      , block_info    = MkBlockInfo
        { field_num1   = 1, is_overflows = 0
        , field_num2   = 2, bucket_num   = -1
        , eof          = 0
        }
      , columns_count
      , rows_count
      }

data BlockInfo = MkBlockInfo
  { field_num1   :: UVarInt, is_overflows :: UInt8
  , field_num2   :: UVarInt, bucket_num   :: Int32
  , eof          :: UVarInt
  }
  deriving (Generic, Serializable)








-- * Server packets

data ServerPacket where
  HelloResponse        :: HelloResponse -> ServerPacket
  DataResponse         :: DataPacket -> ServerPacket
  Exception            :: ExceptionPacket -> ServerPacket
  Progress             :: ProgressPacket -> ServerPacket
  Pong                 :: ServerPacket
  EndOfStream          :: ServerPacket
  ProfileInfo          :: ProfileInfo -> ServerPacket
  Totals               :: ServerPacket
  Extremes             :: ServerPacket
  TablesStatusResponse :: ServerPacket
  Log                  :: ServerPacket
  TableColumns         :: TableColumns -> ServerPacket
  UUIDs                :: ServerPacket
  ReadTaskRequest      :: ServerPacket
  ProfileEvents        :: ServerPacket
  UnknownPacket        :: UVarInt -> ServerPacket

instance Serializable ServerPacket where
  serialize rev = \case
    HelloResponse hello  -> serialize @UVarInt rev 0 <> serialize rev hello
    DataResponse hello   -> serialize @UVarInt rev 1 <> serialize rev hello
    Exception hello      -> serialize @UVarInt rev 2 <> serialize rev hello
    Progress hello       -> serialize @UVarInt rev 3 <> serialize rev hello
    Pong                 -> serialize @UVarInt rev 4
    EndOfStream          -> serialize @UVarInt rev 5
    ProfileInfo hello    -> serialize @UVarInt rev 6 <> serialize rev hello
    Totals               -> serialize @UVarInt rev 7
    Extremes             -> serialize @UVarInt rev 8
    TablesStatusResponse -> serialize @UVarInt rev 9
    Log                  -> serialize @UVarInt rev 10
    TableColumns hello   -> serialize @UVarInt rev 11 <> serialize rev hello
    UUIDs                -> serialize @UVarInt rev 12
    ReadTaskRequest      -> serialize @UVarInt rev 13
    ProfileEvents        -> serialize @UVarInt rev 14
    UnknownPacket num    -> serialize @UVarInt rev num
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
      11 -> TableColumns <$> deserialize rev
      12 -> pure UUIDs
      13 -> pure ReadTaskRequest
      14 -> pure ProfileEvents
      _  -> pure $ UnknownPacket packetNum

serverPacketToNum :: ServerPacket -> UVarInt
serverPacketToNum = \case
  (HelloResponse _) -> 0; (DataResponse _)       -> 1
  (Exception _)     -> 2; (Progress _)           -> 3;
  (Pong)            -> 4; (EndOfStream)          -> 5
  (ProfileInfo _)   -> 6; (Totals)               -> 7
  (Extremes)        -> 8; (TablesStatusResponse) -> 9
  (Log)             -> 10; (TableColumns _)      -> 11;
  (UUIDs)           -> 12; (ReadTaskRequest)     -> 13
  (ProfileEvents)   -> 14; (UnknownPacket num)   -> num


{-
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Client/Connection.cpp#L520
-}
data HelloResponse = MkHelloResponse
  { server_name                    :: ChString
  , server_version_major           :: UVarInt
  , server_version_minor           :: UVarInt
  , server_revision                :: ProtocolRevision
  , server_parallel_replicas_proto :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL
  , server_timezone                :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_SERVER_TIMEZONE
  , server_display_name            :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME
  , server_version_patch           :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , proto_send_chunked_srv         :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , proto_recv_chunked_srv         :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , password_complexity_rules      :: [PasswordComplexityRules] `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES
  , read_nonce                     :: UInt64 `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2
  }
  deriving (Generic, Serializable)

data PasswordComplexityRules = MkPasswordComplexityRules
  { original_pattern  :: ChString
  , exception_message :: ChString
  }
  deriving (Generic, Serializable)


data ExceptionPacket = MkExceptionPacket
  { code        :: Int32
  , name        :: ChString
  , message     :: ChString
  , stack_trace :: ChString
  , nested      :: UInt8
  }
  deriving (Generic, Show, Serializable)

data ProgressPacket = MkProgressPacket
  { rows        :: UVarInt
  , bytes       :: UVarInt
  , total_rows  :: UVarInt
  , total_bytes :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_rows  :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_bytes :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  , elapsed_ns  :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  }
  deriving (Generic, Serializable)

data ProfileInfo = MkProfileInfo
  { rows                         :: UVarInt
  , blocks                       :: UVarInt
  , bytes                        :: UVarInt
  , applied_limit                :: UInt8
  , rows_before_limit            :: UVarInt
  , calculated_rows_before_limit :: UInt8
  , applied_aggregation          :: UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  , rows_before_aggregation      :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  }
  deriving (Generic, Serializable)

data TableColumns = MkTableColumns
  { table_name :: ChString
  , table_columns :: ChString
  }
  deriving (Generic, Serializable)








-- * Client packets

data ClientPacket where
  Hello                     :: HelloPacket -> ClientPacket
  Query                     :: QueryPacket -> ClientPacket
  Data                      :: DataPacket -> ClientPacket
  Cancel                    :: ClientPacket
  Ping                      :: ClientPacket
  TablesStatusRequest       :: ClientPacket
  KeepAlive                 :: ClientPacket
  Scalar                    :: ClientPacket
  IgnoredPartUUIDs          :: ClientPacket
  ReadTaskResponse          :: ClientPacket
  MergeTreeReadTaskResponse :: ClientPacket
  SSHChallengeRequest       :: ClientPacket
  SSHChallengeResponse      :: ClientPacket
  deriving (Generic)

instance Serializable ClientPacket where
  serialize rev packet = case packet of
    (Hello p)                   -> serialize @UVarInt rev 0 <> serialize rev p
    (Query p)                   -> serialize @UVarInt rev 1 <> serialize rev p
    (Data p)                    -> serialize @UVarInt rev 2 <> serialize rev p
    (Cancel)                    -> serialize @UVarInt rev 3
    (Ping)                      -> serialize @UVarInt rev 4
    (TablesStatusRequest)       -> serialize @UVarInt rev 5
    (KeepAlive)                 -> serialize @UVarInt rev 6
    (Scalar)                    -> serialize @UVarInt rev 7
    (IgnoredPartUUIDs)          -> serialize @UVarInt rev 8
    (ReadTaskResponse)          -> serialize @UVarInt rev 9
    (MergeTreeReadTaskResponse) -> serialize @UVarInt rev 10
    (SSHChallengeRequest)       -> serialize @UVarInt rev 11
    (SSHChallengeResponse)      -> serialize @UVarInt rev 12
  deserialize rev = deserialize @UVarInt rev >>= \case
    0 -> Hello <$> deserialize rev
    1 -> Query <$> deserialize rev
    2 -> Data <$> deserialize rev
    3 -> pure Cancel
    4 -> pure Ping
    5 -> pure TablesStatusRequest
    6 -> pure KeepAlive
    7 -> pure Scalar
    8 -> pure IgnoredPartUUIDs
    9 -> pure ReadTaskResponse
    10 -> pure MergeTreeReadTaskResponse
    11 -> pure SSHChallengeRequest
    12 -> pure SSHChallengeResponse
    num -> fail ("Unknown client packet " <> show num)

-- ** Hello

data HelloPacket = MkHelloPacket
  { client_name          :: ChString
  , client_version_major :: UVarInt
  , client_version_minor :: UVarInt
  , tcp_protocol_version :: ProtocolRevision
  , default_database     :: ChString
  , user                 :: ChString
  , pass                 :: ChString
  }
  deriving (Generic, Serializable)

seriliazeHelloPacket :: String -> String -> String -> (ProtocolRevision -> Builder)
seriliazeHelloPacket db user pass =
  flip serialize $ Hello
    MkHelloPacket
      { client_name          = clientName
      , client_version_major = major
      , client_version_minor = minor
      , tcp_protocol_version = latestSupportedRevision
      , default_database     = toChType db
      , user                 = toChType user
      , pass                 = toChType pass
      }


data Addendum = MkAddendum{quota_key :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY}
  deriving (Generic, Serializable)

-- ** Query

data QueryPacket = MkQueryPacket
  { query_id           :: ChString
  , client_info        :: ClientInfo `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_INFO
  , settings           :: DbSettings
  , interserver_secret :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
  , query_stage        :: QueryStage
  , compression        :: UVarInt
  , query              :: ChString
  , parameters         :: QueryParameters `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
  }
  deriving (Generic, Serializable)

data QueryPacketArgs = MkQueryPacketArgs
  { initial_user :: ChString
  , hostname     :: ChString
  , os_user      :: ChString
  , query        :: ChString
  }

serializeQueryPacket :: QueryPacketArgs -> (ProtocolRevision -> Builder)
serializeQueryPacket MkQueryPacketArgs{initial_user, os_user, hostname, query} rev =
  serialize rev $ Query
    MkQueryPacket
      { query_id = ""
      , client_info  = MkSinceRevision MkClientInfo
        { query_kind                   = InitialQuery
        , initial_user
        , initial_query_id             = ""
        , initial_adress               = "0.0.0.0:0"
        , initial_time                 = MkSinceRevision 0
        , interface_type               = 1 -- [tcp - 1, http - 2]
        , os_user
        , hostname
        , client_name                  = clientName
        , client_version_major         = major
        , client_version_minor         = minor
        , client_revision              = rev
        , quota_key                    = MkSinceRevision ""
        , distrubuted_depth            = MkSinceRevision 0
        , client_version_patch         = MkSinceRevision patch
        , open_telemetry               = MkSinceRevision 0
        , collaborate_with_initiator   = MkSinceRevision 0
        , count_participating_replicas = MkSinceRevision 0
        , number_of_current_replica    = MkSinceRevision 0
        }
      , settings           = MkDbSettings []
      , interserver_secret = MkSinceRevision ""
      , query_stage        = Complete
      , compression        = 0
      , query
      , parameters         = MkSinceRevision MkQueryParameters
      }

data DbSettings = MkDbSettings [DbSetting]
data DbSetting = MkDbSetting
  { setting :: ChString
  , flags   :: Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
  , value   :: ChString
  }
instance Serializable DbSettings where
  serialize rev (MkDbSettings _setts) =
    serialize @ChString rev ""
  deserialize _rev =
    fail "DbSettings reading unimplemented"

data QueryParameters = MkQueryParameters
instance Serializable QueryParameters where
  serialize rev _ =
    serialize @ChString rev ""
  deserialize _rev =
    fail "QueryParameters reading unimplemented"

data QueryStage
  = FetchColumns | WithMergeableState | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

instance Serializable QueryStage where
  serialize rev = serialize @UVarInt rev . fromIntegral . fromEnum
  deserialize rev = do
    deserialize @UVarInt rev >>= \case
      0 -> pure FetchColumns
      1 -> pure WithMergeableState
      2 -> pure Complete
      3 -> pure WithMergeableStateAfterAggregation
      4 -> pure WithMergeableStateAfterAggregationAndLimit
      num -> fail ("Unknown QueryStage " <> show num)


data Flags = IMPORTANT | CUSTOM | TIER
_flagCode :: Flags -> UInt64
_flagCode IMPORTANT = 0x01
_flagCode CUSTOM    = 0x02
_flagCode TIER      = 0x0c

data ClientInfo = MkClientInfo
  { query_kind                   :: QueryKind
  , initial_user                 :: ChString
  , initial_query_id             :: ChString
  , initial_adress               :: ChString
  , initial_time                 :: Int64 `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME
  , interface_type               :: UInt8
  , os_user                      :: ChString
  , hostname                     :: ChString
  , client_name                  :: ChString
  , client_version_major         :: UVarInt
  , client_version_minor         :: UVarInt
  , client_revision              :: ProtocolRevision
  , quota_key                    :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
  , distrubuted_depth            :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
  , client_version_patch         :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , open_telemetry               :: UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_OPENTELEMETRY
  , collaborate_with_initiator   :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , count_participating_replicas :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , number_of_current_replica    :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  }
  deriving (Generic, Serializable)

data QueryKind = NoQuery | InitialQuery | SecondaryQuery
instance Serializable QueryKind where
  serialize rev = serialize @UInt8 rev . (\case NoQuery -> 1; InitialQuery -> 2; SecondaryQuery -> 3)
  deserialize rev = deserialize @UInt8 rev >>= \case
      1 -> pure NoQuery
      2 -> pure InitialQuery
      3 -> pure SecondaryQuery
      num -> fail ("Unknown QueryKind " <> show num)
