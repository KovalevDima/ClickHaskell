{-# LANGUAGE 
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DuplicateRecordFields
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , OverloadedStrings
  , TemplateHaskell
  , UndecidableInstances
#-}

module ClickHaskell.NativeProtocol where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Tables ()
import Paths_ClickHaskell (version)

-- GHC included
import Control.DeepSeq (NFData)
import Control.Exception (Exception, throw)
import Data.Binary (Binary (..))
import Data.Binary.Get
import Data.Binary.Get.Internal (readN)
import Data.Bits (Bits (..))
import Data.ByteString as BS (length, take, StrictByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder as BS
  ( byteString
  , int16LE, int32LE, int64LE, int8
  , word16LE, word32LE, word64LE, word8
  )
import Data.ByteString.Lazy.Internal as BL (ByteString(..))
import Data.ByteString.Lazy.Char8 as BSL8 (head)
import Data.ByteString.Lazy (LazyByteString)
import Data.Char (ord)
import Data.Typeable (Proxy (..))
import Data.Vector.Primitive (Prim)
import Data.Version (Version (..), showVersion)
import Data.WideWord (Int128 (..), Word128 (..))
import Data.Word (Word64)
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (lift)
import Data.Text (Text)

-- External
import Network.Socket as Sock (Socket, HostName, ServiceName)
import Network.Socket.ByteString.Lazy (recv)

-- * Connection

-- ** Credentials
data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)


mkHelloPacket :: ChCredential -> HelloPacket
mkHelloPacket MkChCredential{chDatabase,chLogin,chPass} =
  MkHelloPacket
    { packet_type = MkPacket
    , client_name = clientNameAndVersion
    , client_version_major = clientMajorVersion
    , client_version_minor = clientMinorVersion
    , adendum = MkSinceRevision MkAdendum
    , tcp_protocol_version = latestSupportedRevision
    , default_database = toChType chDatabase
    , user = toChType chLogin
    , password = toChType chPass
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


readHelloPacket :: LazyByteString -> ServerHelloResponse
readHelloPacket = runGet (deserialize @ServerHelloResponse latestSupportedRevision)
  -- go (runGetIncremental (deserialize @ServerHelloResponse latestSupportedRevision))

go1 :: Decoder ServerHelloResponse -> IO LazyByteString -> IO ServerHelloResponse
go1 (Partial decoder) getInput                    = do
  input <- getInput
  go1 (decoder . takeHeadChunk $ input) (pure $ dropHeadChunk input)
go1 (Done _leftover _consumed helloPacket) _input = pure helloPacket
go1 (Fail _leftover _consumed msg) _input        = error msg

takeHeadChunk :: LazyByteString -> Maybe StrictByteString
takeHeadChunk lbs =
  case lbs of
    (BL.Chunk bs _) -> Just bs
    _ -> Nothing

dropHeadChunk :: LazyByteString -> LazyByteString
dropHeadChunk lbs =
  case lbs of
    (BL.Chunk _ lbs') -> lbs'
    _ -> BL.Empty




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




-- * Ping

mkPingPacket :: ProtocolRevision -> Builder
mkPingPacket rev = serialize rev MkPingPacket{packet_type = MkPacket}

data PingPacket = MkPingPacket
  { packet_type :: Packet Ping
  }
  deriving (Generic, Serializable)




-- * Querying

-- ** Query

data QueryStage
  = FetchColumns
  | WithMergeableState
  | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

queryStageCode :: QueryStage -> UVarInt
queryStageCode = fromIntegral . fromEnum

data Flags = IMPORTANT | CUSTOM | OBSOLETE
flagCode :: Flags -> ChUInt64
flagCode IMPORTANT = 0x01
flagCode CUSTOM    = 0x02
flagCode OBSOLETE  = 0x04


mkQueryPacket :: ProtocolRevision -> ChString -> ChString -> Builder
mkQueryPacket rev user query = do
  mconcat
    [ serialize @UVarInt rev (packetNumVal @Query)
    , serialize @ChString rev "c6d51fce-a5ad-455d-bfc7-88974c4c2f9d" -- queryId
    , afterRevision @DBMS_MIN_REVISION_WITH_CLIENT_INFO rev
        (mkClientInfo user rev)
    , serialize @ChString rev "" -- settings
    , afterRevision @DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET rev
        (serialize @ChString rev "") -- interserver secret 
    , serialize @UVarInt rev (queryStageCode Complete) 
    , serialize @UVarInt rev 0 -- compression
    , serialize @ChString rev query
    , afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS rev
        (serialize @ChString rev "") -- parameters
    ]

data QueryKind
  = NoQuery
  | InitialQuery
  | SecondaryQuery
  deriving (Enum)

queryKindCode :: QueryKind -> ChUInt8
queryKindCode = fromIntegral . fromEnum

mkClientInfo :: ChString -> ProtocolRevision -> Builder
mkClientInfo user rev =
  mconcat
    [ serialize @ChUInt8 rev (queryKindCode InitialQuery)
    , serialize @ChString rev (toChType user) -- initial user
    , serialize @ChString rev "c6d51fce-a5ad-455d-bfc7-88974c4c2f9d" -- initial query id
    , serialize @ChString rev "0.0.0.0:0" -- initial address
    , afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME rev
        (serialize @ChInt64 rev 0) -- initial time
    , serialize @ChUInt8 rev 1 -- interface type [tcp - 1, http - 2]
    , serialize @ChString rev "dmitry" -- OS user
    , serialize @ChString rev "desktop" -- hostname
    , serialize @ChString rev clientNameAndVersion
    , serialize @UVarInt rev clientMajorVersion
    , serialize @UVarInt rev clientMinorVersion
    , serialize @UVarInt rev rev
    , afterRevision @DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO rev
        (serialize @ChString rev "") -- quota key
    , afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH rev
        (serialize @UVarInt rev 0) -- distrubutedDepth
    , afterRevision @DBMS_MIN_REVISION_WITH_VERSION_PATCH rev
        (serialize @UVarInt rev 7) -- version patch
    , afterRevision @DBMS_MIN_REVISION_WITH_OPENTELEMETRY rev
        (serialize @ChUInt8 rev 0) -- open telemetry
    , afterRevision @DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS rev
        (  serialize @UVarInt rev 0 -- collaborateWith initiator
        <> serialize @UVarInt rev 0 -- count participating replicas
        <> serialize @UVarInt rev 0 -- number of current replica
        )
    ]

clientMajorVersion, clientMinorVersion, clientPatchVersion :: UVarInt
clientMajorVersion = fromIntegral $(lift (versionBranch version !! 0))
clientMinorVersion = fromIntegral $(lift (versionBranch version !! 1))
clientPatchVersion = fromIntegral $(lift (versionBranch version !! 2))

clientNameAndVersion :: ChString
clientNameAndVersion = $(lift ("ClickHaskell-" <> showVersion version))

-- ** Data

type IsScalar = Bool
type DataName = ChString


mkDataPacket :: ProtocolRevision -> DataName -> IsScalar -> Builder
mkDataPacket rev dataName isScalar =
  mconcat
    [ if isScalar
      then serialize @UVarInt rev (packetNumVal @Scalar)
      else serialize @UVarInt rev (packetNumVal @Data)
    , serialize @ChString rev dataName
    , mconcat -- block info
      [ serialize @UVarInt rev 1
      , serialize @ChUInt8 rev 0 -- is overflows
      , serialize @UVarInt rev 2
      , serialize @ChInt32 rev (-1) -- bucket num
      , serialize @UVarInt rev 0
      ]
    , serialize @UVarInt rev 0 -- columns count
    , serialize @UVarInt rev 0 -- rows count
    ]


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




-- * Serialization

{- |
  Unsigned variable-length quantity encoding
-}
newtype UVarInt = MkUVarInt Word64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData, Binary)


-- ** Serializable
class Serializable chType
  where
  default serialize :: (Generic chType, GSerializable (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from

instance Serializable UVarInt where
  serialize _ = go
    where
    go i
      | i < 0x80 = word8 (fromIntegral i)
      | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)
instance Serializable ChUInt8 where serialize _ = word8 . fromChType
instance Serializable ChUInt16 where serialize _ = word16LE . fromChType
instance Serializable ChUInt32 where serialize _ = word32LE . fromChType
instance Serializable ChUInt64 where serialize _ = word64LE . fromChType
instance Serializable ChUInt128 where serialize _ = (\(Word128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChInt8 where serialize _ = int8 . fromChType
instance Serializable ChInt16 where serialize _ = int16LE . fromChType
instance Serializable ChInt32 where serialize _ = int32LE . fromChType
instance Serializable ChInt64 where serialize _ = int64LE . fromChType
instance Serializable ChInt128 where serialize _ = (\(Int128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChString where
  serialize revision str
    =  (serialize @UVarInt revision . fromIntegral . BS.length . fromChType) str
    <> (BS.byteString . fromChType @_ @StrictByteString) str

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
  gSerialize rev ((left :*: left2) :*: right) = gSerialize rev (left :*: (left2 :*: right))

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
  gSerialize rev (left :*: right) = (serialize rev . unK1 . unM1 $ left) <> gSerialize rev right




-- ** Deserializable

class
  Deserializable chType
  where
  default deserialize :: (Generic chType, GDeserializable (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev

instance Deserializable UVarInt where
  deserialize _ = go 0 (0 :: UVarInt)
    where
    go i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else go (i + 1) $! o'
    go _ _ = fail "input exceeds varuint size"
instance Deserializable ChUInt8 where deserialize _ = toChType <$> getWord8
instance Deserializable ChUInt16 where deserialize _ = toChType <$> getWord16le
instance Deserializable ChUInt32 where deserialize _ = toChType <$> getWord32le
instance Deserializable ChUInt64 where deserialize _ = toChType <$> getWord64le
instance Deserializable ChUInt128 where deserialize _ = toChType <$> (Word128 <$> getWord64le <*> getWord64le)
instance Deserializable ChInt8 where deserialize _ = toChType <$> getInt8
instance Deserializable ChInt16 where deserialize _ = toChType <$> getInt16le
instance Deserializable ChInt32 where deserialize _ = toChType <$>  getInt32le
instance Deserializable ChInt64 where deserialize _ = toChType <$>  getInt64le
instance Deserializable ChInt128 where deserialize _ = toChType <$> (Int128 <$> getWord64le <*> getWord64le)
instance Deserializable ChString where
  deserialize revision = do 
    strSize <- fromIntegral <$> deserialize @UVarInt revision
    toChType <$> readN strSize (BS.take strSize)

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
  (Deserializable chType)
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  gDeserialize rev =  M1 . K1 <$> deserialize @chType rev

instance
  (Deserializable chType, GDeserializable right)
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType) :*: right)
  where
  gDeserialize rev = (:*:) <$> (M1 . K1 <$> deserialize @chType rev) <*> gDeserialize rev

instance {-# OVERLAPPING #-}
  (GDeserializable right)
  =>
  GDeserializable (S1 (MetaSel (Just "server_revision") a b f) (Rec0 UVarInt) :*: right)
  where
  gDeserialize rev = do
    server_revision <- deserialize @UVarInt rev
    (:*:) <$> (pure . M1 . K1 $ server_revision) <*> gDeserialize server_revision




-- * Protocol versioning

-- ** Compatibility wrappers

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
