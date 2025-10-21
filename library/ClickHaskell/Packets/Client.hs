module ClickHaskell.Packets.Client where

-- Internal
import ClickHaskell.Primitive
import ClickHaskell.Packets.Data (DataPacket)
import ClickHaskell.Packets.Settings (DbSettings (..))

-- GHC
import Data.Int
import GHC.Generics
import Data.Binary.Builder (Builder)
import Data.ByteString as BS (null)


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


data Addendum = MkAddendum
  { quota_key :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY
  , proto_send_chunked :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , proto_recv_chunked :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , replicas_version :: ProtocolRevision `SinceRevision` DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL
  }
  deriving (Generic, Serializable)

mkAddendum :: Addendum
mkAddendum = MkAddendum
  { quota_key          = MkSinceRevision ""
  , proto_send_chunked = MkSinceRevision "notchunked"
  , proto_recv_chunked = MkSinceRevision "notchunked"
  , replicas_version   = MkSinceRevision $ mkRev @DBMS_PARALLEL_REPLICAS_PROTOCOL_VERSION
  }

-- ** Query

data QueryPacket = MkQueryPacket
  { query_id           :: ChString
  , client_info        :: ClientInfo `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_INFO
  , settings           :: DbSettings
  , external_roles     :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_INTERSERVER_EXTERNALLY_GRANTED_ROLES
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
        , script_query_number          = MkSinceRevision 0
        , script_line_number           = MkSinceRevision 0
        , jwt                          = MkSinceRevision (MkJwt "")
        }
      , settings           = MkDbSettings []
      , interserver_secret = MkSinceRevision ""
      , query_stage        = Complete
      , compression        = 0
      , query
      , parameters         = MkSinceRevision MkQueryParameters
      , external_roles     = MkSinceRevision 0
      }


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
  , script_query_number          :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_QUERY_AND_LINE_NUMBERS
  , script_line_number           :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_QUERY_AND_LINE_NUMBERS
  , jwt                          :: Jwt `SinceRevision` DBMS_MIN_REVISON_WITH_JWT_IN_INTERSERVER
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

data Jwt = MkJwt ChString
instance Serializable Jwt where
  serialize rev (MkJwt str@(MkChString bs)) =
    if not (BS.null bs)
    then serialize @UInt8 rev 1 <> serialize rev str
    else serialize @UInt8 rev 0
  deserialize rev = do
    has_jwt <- deserialize @UInt8 rev
    if has_jwt == 1
    then MkJwt <$> deserialize rev
    else pure $ MkJwt ""
