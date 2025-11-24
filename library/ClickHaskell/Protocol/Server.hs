module ClickHaskell.Protocol.Server where

-- Internal
import ClickHaskell.Primitive
import ClickHaskell.Protocol.Data (DataPacket(..))
import ClickHaskell.Protocol.Settings (DbSettings)

-- GHC
import Data.Int
import GHC.Generics
import ClickHaskell.Columns (Column, deserializeColumn, ColumnHeader, serializeColumn, mkHeader)
import Control.Monad (when)

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
  ProfileEvents        :: ProfileEventsPacket -> ServerPacket
  UnknownPacket        :: UVarInt -> ServerPacket

instance Serializable ServerPacket where
  serialize rev packet = case packet of
    HelloResponse p      -> serialize @UVarInt rev 0 <> serialize rev p
    DataResponse p       -> serialize @UVarInt rev 1 <> serialize rev p
    Exception p          -> serialize @UVarInt rev 2 <> serialize rev p
    Progress p           -> serialize @UVarInt rev 3 <> serialize rev p
    Pong                 -> serialize @UVarInt rev 4
    EndOfStream          -> serialize @UVarInt rev 5
    ProfileInfo p        -> serialize @UVarInt rev 6 <> serialize rev p
    Totals               -> serialize @UVarInt rev 7
    Extremes             -> serialize @UVarInt rev 8
    TablesStatusResponse -> serialize @UVarInt rev 9
    Log                  -> serialize @UVarInt rev 10
    TableColumns p       -> serialize @UVarInt rev 11 <> serialize rev p
    UUIDs                -> serialize @UVarInt rev 12
    ReadTaskRequest      -> serialize @UVarInt rev 13
    ProfileEvents p      -> serialize @UVarInt rev 14 <> serialize rev p
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
      14 -> ProfileEvents <$> deserialize rev
      _  -> pure $ UnknownPacket packetNum

serverPacketToNum :: ServerPacket -> UVarInt
serverPacketToNum p = case p of
  (HelloResponse _) -> 0; (DataResponse _)       -> 1
  (Exception _)     -> 2; (Progress _)           -> 3;
  (Pong)            -> 4; (EndOfStream)          -> 5
  (ProfileInfo _)   -> 6; (Totals)               -> 7
  (Extremes)        -> 8; (TablesStatusResponse) -> 9
  (Log)             -> 10; (TableColumns _)      -> 11;
  (UUIDs)           -> 12; (ReadTaskRequest)     -> 13
  (ProfileEvents _) -> 14; (UnknownPacket num)   -> num


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

data ProfileEventsPacket = MkProfileEventsPacket
  { dataPacket :: DataPacket
  , host_name :: [ChString]
  , current_time :: [DateTime ""]
  , thread_id :: [UInt64]
  , type_ :: [Int8]
  , name :: [ChString]
  , value :: [UInt64]
  } deriving (Generic)

-- ToDo: Simplify
instance Serializable ProfileEventsPacket where
  serialize rev MkProfileEventsPacket{..}
    =  serialize rev dataPacket
    <> serialize rev (mkHeader @(Column "host_name" ChString)) <> serializeColumn @(Column "host_name" ChString) rev id host_name
    <> serialize rev (mkHeader @(Column "current_time" (DateTime ""))) <> serializeColumn @(Column "current_time" (DateTime "")) rev id current_time
    <> serialize rev (mkHeader @(Column "thread_id" UInt64)) <> serializeColumn @(Column "thread_id" UInt64) rev id thread_id
    <> serialize rev (mkHeader @(Column "type" Int8)) <> serializeColumn @(Column "type" Int8) rev id type_
    <> serialize rev (mkHeader @(Column "name" ChString)) <> serializeColumn @(Column "name" ChString) rev id name
    <> serialize rev (mkHeader @(Column "value" UInt64)) <> serializeColumn @(Column "value" UInt64) rev id value
  deserialize rev = do
    dataPacket@MkDataPacket{rows_count, columns_count} <- deserialize rev
    validateColumnsCount columns_count
    !host_name    <- deserialize @ColumnHeader rev *> deserializeColumn @(Column "host_name" ChString) rev rows_count id
    !current_time <- deserialize @ColumnHeader rev *> deserializeColumn @(Column "current_time" (DateTime "")) rev rows_count id
    !thread_id    <- deserialize @ColumnHeader rev *> deserializeColumn @(Column "thread_id" UInt64) rev rows_count id
    !type_        <- deserialize @ColumnHeader rev *> deserializeColumn @(Column "type" Int8) rev rows_count id
    !name         <- deserialize @ColumnHeader rev *> deserializeColumn @(Column "name" ChString) rev rows_count id
    !value        <- deserialize @ColumnHeader rev *> deserializeColumn @(Column "value" UInt64) rev rows_count id
    pure $ MkProfileEventsPacket{..}
    where
    validateColumnsCount count = when (count /= 6) . fail $
      "Unable to parse ProfileEvents packet. Expected 6 columns but got " <> show count
