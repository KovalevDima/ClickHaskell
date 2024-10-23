{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DuplicateRecordFields
  , LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  , UndecidableInstances
#-}

module ClickHaskell.NativeProtocol
  ( module ClickHaskell.NativeProtocol
  , module ClickHaskell.NativeProtocol.Serialization
  , module ClickHaskell.NativeProtocol.Versioning
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol.Serialization
import ClickHaskell.NativeProtocol.Versioning

-- GHC included
import Control.Exception (Exception, throw)
import Data.Binary.Get
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy.Internal as BL (ByteString(..))
import Data.ByteString.Lazy.Char8 as BSL8 (head)
import Data.ByteString.Lazy (LazyByteString)
import Data.Char (ord)
import Data.Typeable (Proxy (..))
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)
import Data.Text (Text)

-- External
import Network.Socket as Sock (Socket)
import Network.Socket.ByteString.Lazy (recv)

-- * Server packets

data ServerPacketType
  = HelloResponse
  | DataResponse
  | Exception
  | Progress
  | Pong
  | EndOfStream
  | ProfileInfo
  | Totals
  | Extremes
  | TablesStatusResponse
  | Log
  | TableColumns
  | UUIDs
  | ReadTaskRequest
  | ProfileEvents
  deriving (Show, Enum, Bounded)

determineServerPacket :: Socket -> IO ServerPacketType
determineServerPacket sock = do
  headByte <- ord . BSL8.head <$> recv sock 1
  pure $
    if headByte <= fromEnum (maxBound :: ServerPacketType)
    then toEnum headByte
    else throw $ ProtocolImplementationError UnknownPacketType


-- ** Bufferized reading

bufferizedRead :: forall packet . Deserializable packet => ProtocolRevision -> IO LazyByteString -> IO packet
bufferizedRead rev bufferFiller = runBufferReader bufferFiller (runGetIncremental (deserialize @packet rev)) BL.Empty

runBufferReader :: Deserializable packet => IO LazyByteString -> Decoder packet -> LazyByteString -> IO packet
runBufferReader bufferFiller (Partial decoder) (BL.Chunk bs mChunk)
  = runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader bufferFiller (Partial decoder) BL.Empty = do
  bufferFiller >>= \case
    BL.Empty -> fail "Expected more bytes while reading packet" -- ToDo: Pass packet name
    BL.Chunk bs mChunk -> runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader _bufferFiller (Done _leftover _consumed helloPacket) _input = pure helloPacket
runBufferReader _bufferFiller (Fail _leftover _consumed msg) _currentBuffer = error msg


-- ** HelloResponse

{-
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Client/Connection.cpp#L520
-}
data ServerHelloResponse = MkServerHelloResponse
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
  deriving (Generic, Deserializable)

data PasswordComplexityRules = MkPasswordComplexityRules
  { original_pattern :: ChString
  , exception_message :: ChString
  }
  deriving (Generic, Deserializable)

instance Deserializable [PasswordComplexityRules] where
  deserialize rev = do
    _uvarInt <- deserialize @UVarInt rev
    pure []


readHelloPacket :: IO LazyByteString -> IO ServerHelloResponse
readHelloPacket = bufferizedRead latestSupportedRevision




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

packetNumVal :: forall packetType . KnownNat (PacketTypeNumber packetType) => UVarInt
packetNumVal = fromIntegral . natVal $ Proxy @(PacketTypeNumber packetType)

instance
  KnownNat (PacketTypeNumber packetType)
  =>
  Serializable (Packet (packetType :: ClientPacketType)) where
  serialize rev _ = serialize @UVarInt rev (packetNumVal @packetType)


-- ** Hello

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
    , adendum              = MkSinceRevision MkAdendum
    }

data HelloParameters = MkHelloParameters
  { chDatabase :: Text
  , chLogin :: Text
  , chPass :: Text
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
  , adendum              :: Adendum `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM
  }
  deriving (Generic, Serializable)

data Adendum = MkAdendum {}
instance Serializable Adendum where serialize _ MkAdendum{} = "\0"


-- ** Ping

mkPingPacket :: ProtocolRevision -> Builder
mkPingPacket rev = serialize rev MkPingPacket{packet_type = MkPacket}

data PingPacket = MkPingPacket
  { packet_type :: Packet Ping
  }
  deriving (Generic, Serializable)


-- ** Query

mkQueryPacket :: ProtocolRevision -> ChString -> ChString -> QueryPacket
mkQueryPacket chosenRev user query = MkQueryPacket
  { query_packet = MkPacket
  , query_id = ""
  , client_info = MkClientInfo
    { query_kind = InitialQuery
    , initial_user = user
    , initial_query_id = ""
    , initial_adress = "0.0.0.0:0"
    , initial_time = MkSinceRevision 0
    , interface_type = 1 -- [tcp - 1, http - 2]
    , os_user = "dmitry"
    , hostname = "desktop"
    , client_name = clientNameAndVersion
    , client_major = clientMajorVersion
    , client_minor = clientMinorVersion
    , client_revision = chosenRev
    , quota_key = MkSinceRevision ""
    , distrubuted_depth = MkSinceRevision 0
    , client_patch = MkSinceRevision clientPatchVersion
    , open_telemetry = MkSinceRevision 0
    , collaborate_with_initiator = MkSinceRevision 0
    , count_participating_replicas = MkSinceRevision 0
    , number_of_current_replica = MkSinceRevision 0
    }
  , settings = MkDbSettings
  , interserver_secret = MkSinceRevision ""
  , query_stage = Complete
  , compression = 0
  , query = query
  , parameters = MkSinceRevision MkQueryParameters
  }

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

data QueryPacket = MkQueryPacket
  { query_packet       :: Packet Query
  , query_id           :: ChString
  , client_info        :: ClientInfo
  , settings           :: DbSettings
  , interserver_secret :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
  , query_stage        :: QueryStage
  , compression        :: UVarInt
  , query              :: ChString
  , parameters         :: QueryParameters `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
  }
  deriving (Generic, Serializable)

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

data DbSettings = MkDbSettings
instance Serializable DbSettings where
  serialize rev _ = serialize @ChString rev ""

data QueryParameters = MkQueryParameters
instance Serializable QueryParameters where
  serialize rev _ = serialize @ChString rev ""

data QueryKind
  = NoQuery
  | InitialQuery
  | SecondaryQuery
  deriving (Enum)

instance Serializable QueryKind where
  serialize rev = serialize @ChUInt8 rev . fromIntegral . fromEnum


-- ** Data

type DataName = ChString

data DataPacket = MkDataPacket
  { packet_type   :: Packet Data
  , data_name     :: ChString
  , block_info    :: BlockInfo
  , columns_count :: UVarInt
  , rows_count    :: UVarInt
  }
  deriving (Generic, Serializable)

data BlockInfo = MkBlockInfo
  { bi1          :: UVarInt
  , is_overflows :: ChUInt8
  , bi3          :: UVarInt
  , bucket_num   :: ChInt32
  , bi5          :: UVarInt
  }
  deriving (Generic, Serializable)

type ColumnsCount = UVarInt
type RowsCount = UVarInt
mkDataPacket :: ColumnsCount -> RowsCount -> DataName -> DataPacket
mkDataPacket columns rows dataName =
  MkDataPacket
    { packet_type   = MkPacket
    , data_name     = dataName
    , block_info    = MkBlockInfo
      { bi1          = 1
      , is_overflows = 0
      , bi3          = 2
      , bucket_num   = -1
      , bi5          = 0
      }
    , columns_count = columns
    , rows_count    = rows
    }




-- * Errors handling

data ClientError
  = ConnectionError ConnectionError
  | DatabaseException
  | ProtocolImplementationError ProtocolImplementationError
  deriving (Show, Exception)

{- |
  You shouldn't see this exceptions. Please report a bug if it appears
-}
data ProtocolImplementationError
  = UnexpectedPacketType ServerPacketType
  | UnknownPacketType
  | DeserializationError
  deriving (Show, Exception)

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)
