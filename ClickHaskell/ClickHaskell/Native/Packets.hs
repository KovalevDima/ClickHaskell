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
import ClickHaskell.Native.Serialization (Serializable(..), putUVarInt)
import ClickHaskell.Native.Versioning
import Paths_ClickHaskell (version)

-- GHC included
import Control.Monad (void)
import Data.ByteString.Builder as BS (Builder, toLazyByteString)
import Data.ByteString.Char8 as BS8 (toStrict, head)
import Data.Text as Text (Text)
import Data.Version (Version (..), showVersion)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (lift)

-- External
import Network.Socket (HostName, ServiceName, Socket)
import Network.Socket.ByteString (send, recv)

-- * Auth data

data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)

clientMajorVersion, clientMinorVersion :: ChUInt16
clientMajorVersion = fromIntegral $(lift (versionBranch version !! 0))
clientMinorVersion = fromIntegral $(lift (versionBranch version !! 1))

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

clientPackerCode :: ClientPacketType -> ChUInt16
clientPackerCode = fromIntegral . fromEnum


-- ** Hello packet

sendHelloPacket :: Socket -> ChCredential -> IO ()
sendHelloPacket sock MkChCredential{chDatabase, chLogin, chPass}  = do
  void . send sock . toStrict . toLazyByteString . mconcat $
    [ putUVarInt @ChUInt16 (clientPackerCode Hello)
    , serialize @ChString clientNameAndVersion
    , putUVarInt @ChUInt16 clientMajorVersion
    , putUVarInt @ChUInt16 clientMinorVersion
    , putUVarInt @ChUInt16 54_460
    , serialize @ChString (toChType chDatabase)
    , serialize @ChString (toChType chLogin)
    , serialize @ChString (toChType chPass)
    ]




-- ** Query packet

data QueryStage
  = FetchColumns
  | WithMergeableState
  | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

queryStageCode :: QueryStage -> ChUInt16
queryStageCode = fromIntegral . fromEnum


sendQueryPacket :: Socket -> ProtocolRevision -> ChCredential -> ChString -> IO ()
sendQueryPacket sock serverRevision creds  query = do
  (void . send sock . toStrict . toLazyByteString . mconcat)
    [ putUVarInt (clientPackerCode Query)
    , serialize @ChString "1ff-a123" -- queryId
    , if serverRevision >= _DBMS_MIN_REVISION_WITH_CLIENT_INFO
      then mkClientInfo creds serverRevision
      else ""
    , serialize @ChString "" -- settings
    , if serverRevision >= _DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
      then serialize @ChString "" -- interserver secret
      else "" 
    , putUVarInt (queryStageCode Complete) 
    , putUVarInt @ChUInt8 0 -- compression
    , serialize @ChString query
    , if serverRevision >= _DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
      then serialize @ChString "" -- parameters
      else ""
    ]

mkClientInfo :: ChCredential -> ProtocolRevision -> Builder
mkClientInfo MkChCredential{chLogin} revision =
  mconcat
    [ serialize @ChUInt8 1 -- query kind
    , serialize @ChString (toChType chLogin)
    , serialize @ChString "" -- initial query id
    , serialize @ChString "initialAdress" -- initial address
    , if revision >= _DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME
      then serialize @ChInt64 0
      else "" -- initial time
    , serialize @ChUInt8 1 -- interface type [tcp - 1, http - 2]
    , serialize @ChString "" -- OS user
    , serialize @ChString "localhost" -- hostname
    , serialize @ChString clientNameAndVersion
    , putUVarInt @ChUInt16 clientMajorVersion
    , putUVarInt @ChUInt16 clientMinorVersion
    , putUVarInt @ChUInt16 revision
    , if revision >= _DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
      then serialize @ChString "" -- quota key
      else ""
    , if revision >= _DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
      then putUVarInt @ChUInt16 0 -- distrubutedDepth
      else ""
    , if revision >= _DBMS_MIN_REVISION_WITH_VERSION_PATCH
      then putUVarInt @ChUInt16 0 -- version patch
      else ""
    , if revision >= _DBMS_MIN_REVISION_WITH_OPENTELEMETRY
      then serialize @ChUInt8 0 -- open telemetry
      else ""
    , if revision >= _DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
      then putUVarInt @ChUInt16 0 -- collaborateWith initiator
        <> putUVarInt @ChUInt16 0 -- count participating replicas
        <> putUVarInt @ChUInt16 0 -- number of current replica
      else ""
    ]




-- ** Data packet

type IsScalar = Bool
type DataName = ChString


sendDataPacket :: Socket -> ProtocolRevision -> DataName -> IsScalar -> IO ()
sendDataPacket sock serverRevision dataName isScalar =
  (void . send sock . toStrict . toLazyByteString . mconcat)
    [ if isScalar
      then putUVarInt @ChUInt16 (clientPackerCode Scalar)
      else putUVarInt @ChUInt16 (clientPackerCode Data)
    , serialize @ChString dataName
    , mkBlockInfo serverRevision
    , putUVarInt @ChUInt64 0 -- columnsCount
    , putUVarInt @ChUInt64 0 -- rowsCount
    ]

mkBlockInfo :: ProtocolRevision -> Builder
mkBlockInfo _serverRevision = mconcat
  [ putUVarInt @ChUInt16 0
  ]




-- ** Ping packet

sendPingPacket :: Socket -> IO ()
sendPingPacket sock =
  (void . send sock)
    (toStrict . toLazyByteString $ serialize @ChUInt8 4)
