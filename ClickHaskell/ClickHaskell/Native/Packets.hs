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

-- * Auth data

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
  deriving (Enum)

determinePacket :: Socket -> IO (Maybe ServerPacketType)
determinePacket sock = do
  headByte <- fromEnum . BS8.head <$> recv sock 1
  pure $
    if headByte < 15
    then Just (toEnum headByte)
    else Nothing




-- * Client packets handling


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
    , "\0"
    ]




-- ** Query packet

type Query = ChString

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

mkQueryPacket :: ProtocolRevision -> ChCredential -> Query -> Builder
mkQueryPacket serverRevision creds query = do
  mconcat
    [ uVarInt (clientPacketCode Query)
    , serialize @ChString "c6d51fce-a5ad-455d-bfc7-88974c4c2f9d" -- queryId
    , if serverRevision >= _DBMS_MIN_REVISION_WITH_CLIENT_INFO
      then mkClientInfo creds serverRevision
      else ""
    , serialize @ChString "" -- settings
    , if serverRevision >= _DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
      then serialize @ChString "" -- interserver secret
      else "" 
    , uVarInt (queryStageCode Complete) 
    , uVarInt @ChUInt8 0 -- compression
    , serialize @ChString query
    , if serverRevision >= _DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
      then serialize @ChString "" -- parameters
      else ""
    ]

data QueryKind
  = NoQuery
  | InitialQuery
  | SecondaryQuery
  deriving (Enum)

queryKindCode :: QueryKind -> ChUInt8
queryKindCode = fromIntegral . fromEnum

mkClientInfo :: ChCredential -> ProtocolRevision -> Builder
mkClientInfo MkChCredential{{-chLogin-}} revision =
  mconcat
    [ serialize @ChUInt8 (queryKindCode InitialQuery)
    , serialize @ChString "" -- (toChType chLogin) -- initial user
    , serialize @ChString "c6d51fce-a5ad-455d-bfc7-88974c4c2f9d" -- initial query id
    , serialize @ChString "0.0.0.0:0" -- initial address
    , if revision >= _DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME
      then serialize @ChInt64 0
      else "" -- initial time
    , serialize @ChUInt8 1 -- interface type [tcp - 1, http - 2]
    , serialize @ChString "dmitry" -- OS user
    , serialize @ChString "desktop" -- hostname
    , serialize @ChString clientNameAndVersion
    , uVarInt @ChUInt64 clientMajorVersion
    , uVarInt @ChUInt64 clientMinorVersion
    , uVarInt @ChUInt64 revision
    , if revision >= _DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
      then serialize @ChString "" -- quota key
      else ""
    , if revision >= _DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
      then uVarInt @ChUInt16 0 -- distrubutedDepth
      else ""
    , if revision >= _DBMS_MIN_REVISION_WITH_VERSION_PATCH
      then uVarInt @ChUInt16 7 -- version patch
      else ""
    , if revision >= _DBMS_MIN_REVISION_WITH_OPENTELEMETRY
      then serialize @ChUInt8 0 -- open telemetry
      else ""
    , if revision >= _DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
      then uVarInt @ChUInt16 0 -- collaborateWith initiator
        <> uVarInt @ChUInt16 0 -- count participating replicas
        <> uVarInt @ChUInt16 0 -- number of current replica
      else ""
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
    , mkBlockInfo
    , uVarInt @ChUInt64 0 -- columnsCount
    , uVarInt @ChUInt64 0 -- rowsCount
    ]

mkBlockInfo :: Builder
mkBlockInfo = let
  isOverflows = 0
  bucketNum = -1
  in mconcat
    [ uVarInt @ChUInt16 1
    , serialize @ChUInt8 isOverflows
    , uVarInt @ChUInt16 2
    , serialize @ChInt32 bucketNum
    , uVarInt @ChUInt16 0 
    ]




-- ** Ping packet

mkPingPacket :: Builder
mkPingPacket = uVarInt (clientPacketCode Ping)
