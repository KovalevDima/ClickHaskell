{-# LANGUAGE DeriveGeneric, OverloadedStrings, NamedFieldPuns, NumericUnderscores #-}
module ClickHaskell.Native.Packets where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Native.Serialization (Serializable(..), putUVarInt)
import Paths_ClickHaskell (version)

-- GHC included
import Control.Monad (void)
import Data.ByteString.Builder as BS (Builder, toLazyByteString)
import Data.ByteString.Char8 as BS8 (toStrict, head)
import Data.Text as Text (Text)
import Data.Version (Version (..))
import GHC.Generics (Generic)

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


-- * Server packet

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




-- * Client packets

-- ** Hello packet

sendHelloPacket :: Socket -> ChCredential -> IO ()
sendHelloPacket sock MkChCredential{chDatabase, chLogin, chPass} = do
  let helloPacketCode = serialize @ChUInt8 0
      clientName      = serialize @ChString "ClickHaskell"
      majorVersion    = putUVarInt @ChUInt16 (fromIntegral $ versionBranch version !! 0)
      minorVersion    = putUVarInt @ChUInt16 (fromIntegral $ versionBranch version !! 1)
      protocolVersion = putUVarInt @ChUInt16 54_460
      database        = serialize @ChString (toChType chDatabase)
      login           = serialize @ChString (toChType chLogin)
      password        = serialize @ChString (toChType chPass)
  void . send sock . toStrict . toLazyByteString . mconcat $
    [ helloPacketCode
    , clientName, majorVersion, minorVersion
    , protocolVersion
    , database, login, password
    ]




-- ** Query packet


sendQueryPacket :: Socket -> ChCredential -> ChString -> IO ()
sendQueryPacket sock creds query = do
  let queryPacketCode   = serialize @ChUInt8 1
      queryId           = serialize @ChString "1ff-a123"
      clientInfo        = mkClientInfo creds
      settings          = serialize @ChString "" -- No settings
      interserverSecret = serialize @ChString ""
      stage             = putUVarInt @ChUInt8 2
      compression       = putUVarInt @ChUInt8 0
      body              = serialize @ChString query
      parameters        = serialize @ChString "" -- No parameters
  (void . send sock . toStrict . toLazyByteString . mconcat)
    $  [ queryPacketCode
       , queryId
       ]
    <> clientInfo
    <> [settings]
    <> [interserverSecret]
    <> [stage, compression, body]
    <> [parameters]

mkClientInfo :: ChCredential -> [Builder]
mkClientInfo MkChCredential{chLogin} =
  let queryKind                  = serialize @ChUInt8 1
      initialUser                = serialize @ChString (toChType chLogin)
      initialQueryId             = serialize @ChString ""
      initialAddress             = serialize @ChString "initialAdress"
      initialTime                = serialize @ChInt64 0
      interfaceType              = serialize @ChUInt8 1 -- [tcp - 1, http - 2]
      osUser                     = serialize @ChString ""
      hostname                   = serialize @ChString "localhost"
      clientName                 = serialize @ChString "ClickHaskell"
      majorVersion               = putUVarInt @ChUInt16 (fromIntegral $ versionBranch version !! 0)
      minorVersion               = putUVarInt @ChUInt16 (fromIntegral $ versionBranch version !! 1)
      protocolVersion            = putUVarInt @ChUInt16 54_485
      quotaKey                   = serialize @ChString ""
      distrubutedDepth           = putUVarInt @ChUInt16 0
      versionPatch               = putUVarInt @ChUInt16 0
      openTelemetry              = serialize @ChUInt8 0
      collaborateWithInitiator   = putUVarInt @ChUInt16 0
      countParticipatingReplicas = putUVarInt @ChUInt16 0
      numberOfCurrentReplica     = putUVarInt @ChUInt16 0
  in
    [ queryKind
    , initialUser
    , initialQueryId
    , initialAddress
    , initialTime
    , interfaceType
    , osUser, hostname
    , clientName
    , majorVersion
    , minorVersion
    , protocolVersion
    , quotaKey
    , distrubutedDepth
    , versionPatch, openTelemetry
    , collaborateWithInitiator
    , countParticipatingReplicas
    , numberOfCurrentReplica
    ]





-- ** Data packet

sendDataPacket :: Socket -> IO ()
sendDataPacket sock =
  let dataPacketCode = serialize @ChUInt8 2
      columns        = serialize @ChString ""
      blockInfo      = mkBlockInfo
      columnsCount   = putUVarInt @ChUInt64 0
      rowsCount      = putUVarInt @ChUInt64 0
  in
  (void . send sock . toStrict . toLazyByteString . mconcat)
    $  [dataPacketCode, columns]
    <> blockInfo
    <> [columnsCount, rowsCount]

mkBlockInfo :: [Builder]
mkBlockInfo =
  [ serialize @ChUInt8 1 -- [Scalar - 1, Data - 2]
  , serialize @ChUInt8 0
  , putUVarInt @ChUInt16 2
  , serialize @ChInt32 (-1)
  , putUVarInt @ChUInt16 0
  ]




-- ** Ping packet

sendPingPacket :: Socket -> IO ()
sendPingPacket sock =
  (void . send sock)
    (toStrict . toLazyByteString $ serialize @ChUInt8 4)
