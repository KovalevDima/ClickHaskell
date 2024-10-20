{-# LANGUAGE 
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , TemplateHaskell
#-}

module ClickHaskell.NativeProtocol where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Tables ()
import Paths_ClickHaskell (version)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.ByteString.Builder (toLazyByteString, Builder)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.ByteString.Lazy.Char8 as BSL8 (head)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import System.Timeout (timeout)
import Data.Version (Version (..), showVersion)
import Language.Haskell.TH.Syntax (lift)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.ByteString as BS (length)
import Data.ByteString.Builder as BS (byteString, int16LE, int32LE, int64LE, int8, word64LE, word8, word32LE, word16LE)
import Data.ByteString.Char8 as BS8 (ByteString)
import Data.Bits (Bits (..))
import Data.WideWord (Int128(..), Word128 (..))

-- External
import Network.Socket as Sock
import Network.Socket.ByteString.Lazy (recv, sendAll)


-- * Connection

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)

data Connection = MkConnection
  { sock :: Socket
  , user :: User
  }

openNativeConnection :: ChCredential -> IO Connection
openNativeConnection credentials@MkChCredential{chHost, chPort, chLogin} = do
  AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress}
    <- fromMaybe (throw $ ConnectionError NoAdressResolved) . listToMaybe
    <$> getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just chHost)
      (Just chPort)
  sock <- (fromMaybe (throw $ ConnectionError EstablishTimeout) <$>) . timeout 3_000_000 $
    bracketOnError
      (socket addrFamily addrSocketType addrProtocol)
      (\sock ->
        catch @SomeException
          (finally
            (shutdown sock ShutdownBoth)
            (close sock)
          )
          (const $ pure ())
      )
      (\sock -> do
         setSocketOption sock NoDelay 1
         setSocketOption sock Sock.KeepAlive 1
         connect sock addrAddress
         pure sock
      )

  (sendAll sock . toLazyByteString) (mkHelloPacket latestSupportedRevision credentials)
  serverPacketType <- determineServerPacket sock
  _ <- recv sock 4096
  case serverPacketType of
    Just HelloResponse -> pure $ MkConnection{sock, user=chLogin}
    Just Exception -> throw DatabaseException
    Just otherPacket -> throw . ProtocolImplementationError $ UnexpectedPacketType otherPacket
    Nothing -> throw . ProtocolImplementationError $ UnknownPacketType

data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)

mkHelloPacket :: ProtocolRevision -> ChCredential -> Builder
mkHelloPacket clientRevision MkChCredential{chDatabase, chLogin, chPass}  = do
  mconcat
    [ uVarInt (clientPacketCode Hello)
    , serialize @ChString clientNameAndVersion
    , uVarInt @ChUInt64 clientMajorVersion
    , uVarInt @ChUInt64 clientMinorVersion
    , uVarInt @ChUInt64 clientRevision
    , serialize @ChString (toChType chDatabase)
    , serialize @ChString (toChType chLogin)
    , serialize @ChString (toChType chPass)
    , afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM clientRevision
        "\0"
    ]

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
  deriving (Show, Enum)

determineServerPacket :: Socket -> IO (Maybe ServerPacketType)
determineServerPacket sock = do
  headByte <- fromEnum . BSL8.head <$> recv sock 1
  pure $
    if headByte < 15
    then Just (toEnum headByte)
    else Nothing

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
  deriving (Enum)

clientPacketCode :: ClientPacketType -> ChUInt64
clientPacketCode = fromIntegral . fromEnum





-- * Ping

ping :: Connection -> IO ()
ping MkConnection{sock} = do
  (sendAll sock . toLazyByteString) mkPingPacket
  responscePacket <- determineServerPacket sock
  case responscePacket of
    Just Pong -> pure ()
    Just Exception -> throw DatabaseException
    Just otherPacket -> throw . ProtocolImplementationError $ UnexpectedPacketType otherPacket
    Nothing -> throw . ProtocolImplementationError $ UnknownPacketType

mkPingPacket :: Builder
mkPingPacket =
  uVarInt (clientPacketCode Ping)




-- * Querying

selectFrom :: Connection -> IO ()
selectFrom MkConnection{sock, user} = do
  (sendAll sock . toLazyByteString)
    (  mkQueryPacket latestSupportedRevision user "SELECT 5"
    <> mkDataPacket "" False
    )
  _ <- recv sock 4096
  pure ()

-- ** Query

data QueryStage
  = FetchColumns
  | WithMergeableState
  | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)
queryStageCode :: QueryStage -> ChUInt16
queryStageCode = fromIntegral . fromEnum

data Flags = IMPORTANT | CUSTOM | OBSOLETE
flagCode :: Flags -> ChUInt64
flagCode IMPORTANT = 0x01
flagCode CUSTOM    = 0x02
flagCode OBSOLETE  = 0x04


type Query = ChString
type User = Text

mkQueryPacket :: ProtocolRevision -> User -> Query -> Builder
mkQueryPacket serverRevision creds query = do
  mconcat
    [ uVarInt (clientPacketCode Query)
    , serialize @ChString "c6d51fce-a5ad-455d-bfc7-88974c4c2f9d" -- queryId
    , afterRevision @DBMS_MIN_REVISION_WITH_CLIENT_INFO serverRevision
        (mkClientInfo creds serverRevision)
    , serialize @ChString "" -- settings
    , afterRevision @DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET serverRevision
        (serialize @ChString "") -- interserver secret 
    , uVarInt (queryStageCode Complete) 
    , uVarInt @ChUInt8 0 -- compression
    , serialize @ChString query
    , afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS serverRevision
        (serialize @ChString "") -- parameters
    ]

data QueryKind
  = NoQuery
  | InitialQuery
  | SecondaryQuery
  deriving (Enum)

queryKindCode :: QueryKind -> ChUInt8
queryKindCode = fromIntegral . fromEnum

mkClientInfo :: User -> ProtocolRevision -> Builder
mkClientInfo user revision =
  mconcat
    [ serialize @ChUInt8 (queryKindCode InitialQuery)
    , serialize @ChString (toChType user) -- initial user
    , serialize @ChString "c6d51fce-a5ad-455d-bfc7-88974c4c2f9d" -- initial query id
    , serialize @ChString "0.0.0.0:0" -- initial address
    , afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME revision
        (serialize @ChInt64 0) -- initial time
    , serialize @ChUInt8 1 -- interface type [tcp - 1, http - 2]
    , serialize @ChString "dmitry" -- OS user
    , serialize @ChString "desktop" -- hostname
    , serialize @ChString clientNameAndVersion
    , uVarInt @ChUInt64 clientMajorVersion
    , uVarInt @ChUInt64 clientMinorVersion
    , uVarInt @ChUInt64 revision
    , afterRevision @DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO revision
        (serialize @ChString "") -- quota key
    , afterRevision @DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH revision
        (uVarInt @ChUInt16 0) -- distrubutedDepth
    , afterRevision @DBMS_MIN_REVISION_WITH_VERSION_PATCH revision
        (uVarInt @ChUInt16 7) -- version patch
    , afterRevision @DBMS_MIN_REVISION_WITH_OPENTELEMETRY revision
        (serialize @ChUInt8 0) -- open telemetry
    , afterRevision @DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS revision
        (  uVarInt @ChUInt16 0 -- collaborateWith initiator
        <> uVarInt @ChUInt16 0 -- count participating replicas
        <> uVarInt @ChUInt16 0 -- number of current replica
        )
    ]

clientMajorVersion, clientMinorVersion, clientPatchVersion :: ChUInt64
clientMajorVersion = fromIntegral $(lift (versionBranch version !! 0))
clientMinorVersion = fromIntegral $(lift (versionBranch version !! 1))
clientPatchVersion = fromIntegral $(lift (versionBranch version !! 2))

clientNameAndVersion :: ChString
clientNameAndVersion = $(lift ("ClickHaskell-" <> showVersion version))

-- ** Data

type IsScalar = Bool
type DataName = ChString


mkDataPacket :: DataName -> IsScalar -> Builder
mkDataPacket dataName isScalar =
  mconcat
    [ if isScalar
      then uVarInt (clientPacketCode Scalar)
      else uVarInt (clientPacketCode Data)
    , serialize @ChString dataName
    , mconcat -- block info
      [ uVarInt @ChUInt16 1
      , serialize @ChUInt8 0 -- is overflows
      , uVarInt @ChUInt16 2
      , serialize @ChInt32 (-1) -- bucket num
      , uVarInt @ChUInt16 0 
      ]
    , uVarInt @ChUInt64 0 -- columns count
    , uVarInt @ChUInt64 0 -- rows count
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
  deriving (Show, Exception)


-- * Serialization


{- |
  Unsigned variable-length quantity encoding
-}
uVarInt :: (Bits a, Num a, Integral a) => a -> Builder
uVarInt = go
  where
  go i
    | i < 0x80 = word8 (fromIntegral i)
    | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)
{-# SPECIALIZE uVarInt :: ChUInt8 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt16 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt32 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt64 -> Builder #-}
{-# SPECIALIZE uVarInt :: ChUInt128 -> Builder #-}




class Serializable chType where serialize :: chType -> Builder
instance Serializable ChUInt8 where serialize = word8 . fromChType
instance Serializable ChUInt16 where serialize = word16LE . fromChType
instance Serializable ChUInt32 where serialize = word32LE . fromChType
instance Serializable ChUInt64 where serialize = word64LE . fromChType
instance Serializable ChUInt128 where serialize = (\(Word128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChInt8 where serialize = int8 . fromChType
instance Serializable ChInt16 where serialize = int16LE . fromChType
instance Serializable ChInt32 where serialize = int32LE . fromChType
instance Serializable ChInt64 where serialize = int64LE . fromChType
instance Serializable ChInt128 where serialize = (\(Int128 hi lo) -> word64LE hi <> word64LE lo) . fromChType
instance Serializable ChString where
  serialize str
    =  (uVarInt @ChUInt64 . fromIntegral . BS.length . fromChType) str
    <> (BS.byteString . fromChType @_ @ByteString) str


class Deserializable chType where deserialize :: ByteString -> chType



-- * Protocol versioning

type ProtocolRevision = ChUInt64

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


{-
  ** Slightly modified
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
