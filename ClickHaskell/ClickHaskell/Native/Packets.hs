{-# LANGUAGE DeriveGeneric, OverloadedStrings, NamedFieldPuns, NumericUnderscores #-}
module ClickHaskell.Native.Packets where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.Native.Serialization (Serializable(..))
import Paths_ClickHaskell (version)

-- GHC included
import Control.Monad (void)
import Data.ByteString.Builder as BS (Builder, toLazyByteString)
import Data.ByteString.Char8 as BS8 (toStrict)
import Data.Text as Text (Text)
import Data.Version (Version (..))
import GHC.Generics (Generic)

-- External
import Network.Socket (HostName, ServiceName, Socket)
import Network.Socket.ByteString (recv, send)

-- * Auth data

data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)




-- * Hello packet

sendHelloPacket :: Socket -> ChCredential -> IO ()
sendHelloPacket sock MkChCredential{chDatabase, chLogin, chPass} = do
  let helloPacketCode = serialize @ChUInt8 0
      clientName      = serialize @ChString "ClickHaskell"
      majorVersion    = serialize @ChUInt16 (fromIntegral $ versionBranch version !! 0)
      minorVersion    = serialize @ChUInt16 (fromIntegral $ versionBranch version !! 1)
      protocolVersion = serialize @ChUInt16 55_255
      database        = serialize @ChString (toChType chDatabase)
      login           = serialize @ChString (toChType chLogin)
      password        = serialize @ChString (toChType chPass)
  void . send sock . toStrict . toLazyByteString . mconcat $
    [ helloPacketCode
    , clientName, majorVersion, minorVersion
    , protocolVersion
    , database, login, password
    ]
  bs <- recv sock 4096
  print bs




-- * Query

sendQueryPacket :: Socket -> ChCredential -> ChString -> IO ()
sendQueryPacket sock creds query = do
  let queryId           = serialize @ChString "id"
      interserverSecret = serialize @ChString ""
      stage             = serialize @ChUInt16 2
      compression       = serialize @ChUInt16 0
      body              = serialize @ChString query
  _sentSize <- send sock
    (toStrict . toLazyByteString . mconcat $
      [queryId]
      <> clientInfo creds
      <> settings
      <>
        [ interserverSecret
        , stage
        , compression
        , body
        ]
    )
  bs <- recv sock 4096
  print bs


clientInfo :: ChCredential -> [Builder]
clientInfo MkChCredential{chLogin} =
  let queryKind                  = serialize @ChUInt8 1
      initialUser                = serialize @ChString (toChType chLogin)
      initialQueryId             = serialize @ChString ""
      initialAddress             = serialize @ChString "initialAdress"
      initialQueryStartTime      = serialize @ChUInt64 0
      interfaceType              = serialize @ChUInt8 1
      osUser                     = serialize @ChString "example"
      hostname                   = serialize @ChString "localhost"
      clientName                 = serialize @ChString "ClickHaskell"
      majorVersion               = serialize @ChUInt16 (fromIntegral $ versionBranch version !! 0)
      minorVersion               = serialize @ChUInt16 (fromIntegral $ versionBranch version !! 1)
      protocolVersion            = serialize @ChUInt16 55_255
      quotaKey                   = serialize @ChString ""
      distrubutedDepth           = serialize @ChUInt16 0
      versionPatch               = serialize @ChUInt16 0
      openTelemetry              = serialize @ChUInt8 0
      collaborateWithInitiator   = serialize @ChUInt16 0
      countParticipatingReplicas = serialize @ChUInt16 0
      numberOfCurrentReplica     = serialize @ChUInt16 0
  in
    [ queryKind
    , initialUser, initialQueryId, initialAddress, initialQueryStartTime, interfaceType
    , osUser, hostname
    , clientName
    , majorVersion, minorVersion
    , protocolVersion
    , quotaKey, distrubutedDepth
    , versionPatch, openTelemetry
    , collaborateWithInitiator
    , countParticipatingReplicas, numberOfCurrentReplica
    ]

settings :: [Builder]
settings =
  let settingsFlag = serialize @ChUInt16 0x01
  in
  [ settingsFlag
  ]
