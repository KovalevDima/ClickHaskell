{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, NamedFieldPuns, NumericUnderscores #-}
module ClickHaskell.Native where

-- Internal dependencies
import ClickHaskell.DbTypes
import Paths_ClickHaskell (version)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Control.Monad (void)
import Data.ByteString as BS (length)
import Data.ByteString.Builder as BS (Builder, byteString, int16LE, int32LE, int64LE, int8, toLazyByteString, word64LE, word8)
import Data.ByteString.Char8 as BS8 (ByteString, toStrict)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text as Text (Text)
import Data.Version (Version (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Bits (..), Ptr)
import GHC.Generics (Generic)
import Network.Socket
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString (recv, send)
import Data.WideWord (Int128(..))

openNativeConnection :: ChCredential -> IO (Socket, SockAddr)
openNativeConnection MkChCredential{chHost, chPort} = do
  AddrInfo
    { addrFamily
    , addrSocketType
    , addrProtocol
    , addrAddress
    } <-
      fromMaybe (throw NoAdressResolved)
    . listToMaybe
    <$>
    getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just chHost)
      (Just chPort)

  (fromMaybe (throw EstablishTimeout) <$>) . timeout 3_000_000 $
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
         setSocketOption sock KeepAlive 1
         connect sock addrAddress
         pure (sock, addrAddress)
      )

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = "default"
  , chHost = "localhost"
  , chPort = "9000"
  }


data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)

class
  Deserializable chType
  where
  deserialize :: Ptr Word8 -> chType

class Serializable chType
  where
  serialize :: chType -> Builder

instance Serializable ChUInt8 where
  serialize = word8 . fromChType @ChUInt8 @Word8

instance Serializable ChUInt16 where
  serialize = vlq128 . fromChType @ChUInt16 @Word16

instance Serializable ChUInt32 where
  serialize = vlq128 . fromChType @ChUInt32 @Word32

instance Serializable ChUInt64 where
  serialize = vlq128 . fromChType @ChUInt64 @Word64

instance Serializable ChUInt128 where
  serialize = vlq128 . fromChType @ChUInt128 @Word128
instance Serializable ChInt8 where
  serialize = int8 . fromChType @ChInt8 @Int8

instance Serializable ChInt16 where
  serialize = int16LE . fromChType @ChInt16 @Int16

instance Serializable ChInt32 where
  serialize = int32LE . fromChType @ChInt32 @Int32

instance Serializable ChInt64 where
  serialize = int64LE . fromChType @ChInt64 @Int64

instance Serializable ChInt128 where
  serialize = (\w128 -> word64LE (int128Hi64 w128) <> word64LE (int128Lo64 w128)) . fromChType @ChInt128 @Int128

instance Serializable ChString where
  serialize str
    =  (serialize @ChUInt64 . fromIntegral . BS.length . fromChType @_ @ByteString) str
    <> (BS.byteString . fromChType @_ @ByteString) str

dev :: IO ()
dev = do
  (sock, _sockAddr) <- openNativeConnection devCredential
  sendHelloPacket sock devCredential
  sendQueryPacket sock devCredential "SELECT 5"


{-# SPECIALIZE vlq128 :: Word8 -> Builder #-}
{-# SPECIALIZE vlq128 :: Word16 -> Builder #-}
{-# SPECIALIZE vlq128 :: Word32 -> Builder #-}
{-# SPECIALIZE vlq128 :: Word64 -> Builder #-}
vlq128 :: (Bits a, Num a, Integral a) => a -> Builder
vlq128 = go
  where
  go i
    | i <= 127  = word8 (fromIntegral i :: Word8)
    | otherwise = word8 (0x80 .&. (fromIntegral i .|. 0x7F)) <> go (i `unsafeShiftR` 7)



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

sendQueryPacket :: Socket -> ChCredential -> ChString -> IO ()
sendQueryPacket sock creds query = do
  let queryId           = serialize @ChString "id"
      body              = serialize @ChString query
      interserverSecret = serialize @ChString ""
      stage             = serialize @ChUInt16 2
      compression       = serialize @ChUInt16 0
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
    , initialUser
    , initialQueryId
    , initialAddress
    , initialQueryStartTime
    , interfaceType
    , osUser
    , hostname
    , clientName
    , majorVersion
    , minorVersion
    , protocolVersion
    , quotaKey
    , distrubutedDepth
    , versionPatch
    , openTelemetry
    , collaborateWithInitiator
    , countParticipatingReplicas
    , numberOfCurrentReplica
    ]

settings :: [Builder]
settings =
  let settingsFlag = serialize @ChUInt16 0x01
  in
  [ settingsFlag
  ]
