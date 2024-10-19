{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , NamedFieldPuns
  , NumericUnderscores
  , TemplateHaskell
#-}

module ClickHaskell.Native.Packets where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Native.Serialization (Serializable(..), uVarInt)
import ClickHaskell.Native.Versioning
import Paths_ClickHaskell (version)

-- GHC included
import Data.ByteString.Builder as BS (Builder)
import Data.ByteString.Char8 as BS8 ( head)
import Data.Text as Text (Text)
import Data.Version (Version (..), showVersion)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (lift)

-- External
import Network.Socket (HostName, ServiceName, Socket)
import Network.Socket.ByteString (recv)

-- * Protocol parts

-- ** Auth data

data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)

clientMajorVersion, clientMinorVersion, clientPatchVersion :: ChUInt64
clientMajorVersion = fromIntegral $(lift (versionBranch version !! 0))
clientMinorVersion = fromIntegral $(lift (versionBranch version !! 1))
clientPatchVersion = fromIntegral $(lift (versionBranch version !! 2))

clientNameAndVersion :: ChString
clientNameAndVersion = $(lift ("ClickHaskell-" <> showVersion version))

-- ** Server packets

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
  headByte <- fromEnum . BS8.head <$> recv sock 1
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




-- * Client packets handling

-- ** Hello packet

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




-- ** Query packet

type Query = ChString
type User = Text

data QueryStage
  = FetchColumns
  | WithMergeableState
  | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

queryStageCode :: QueryStage -> ChUInt16
queryStageCode = fromIntegral . fromEnum


data Flags
  = IMPORTANT
  | CUSTOM
  | OBSOLETE

flagCode :: Flags -> ChUInt64
flagCode IMPORTANT = 0x01
flagCode CUSTOM    = 0x02
flagCode OBSOLETE  = 0x04

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




-- ** Data packet

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




-- ** Ping packet

mkPingPacket :: Builder
mkPingPacket =
  uVarInt (clientPacketCode Ping)
