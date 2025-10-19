module ClickHaskell.Packets.Server where

-- Internal
import ClickHaskell.Primitive
import ClickHaskell.Packets.Data (DataPacket)
import ClickHaskell.Packets.Client (DbSettings)

-- GHC
import Data.Int
import GHC.Generics


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
  , settings                       :: DbSettings `SinceRevision` DBMS_MIN_REVISION_WITH_SERVER_SETTINGS
  , server_query_plan_serialization_version :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_QUERY_PLAN_SERIALIZATION
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
