{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DuplicateRecordFields
  , GADTs
  , GeneralizedNewtypeDeriving
  , ImportQualifiedPost
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , RecordWildCards
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , StandaloneDeriving
  , UndecidableInstances
#-}

{-# OPTIONS_GHC
  -Wno-orphans
  -Wno-unused-top-binds
  -Wno-unused-imports
#-}

module ClickHaskell
  (
  -- * Connection
    ChCredential(..), defaultCredentials
  , Connection(..), openNativeConnection

  -- * Reading and writing
  , Table
  , Columns, Column, KnownColumn(..), DeserializableColumn(..)

  -- * Reading
  , ReadableFrom(..), FromChType(fromChType)
  , select
  , selectFrom
  , selectFromView, View, ToQueryPart(toQueryPart), parameter, Parameter, Parameters, viewParameters
  , generateRandom
  -- ** Internal
  , handleSelect

  -- * Errors
  , ClientError(..)
  , ConnectionError(..)
  , UserError(..)
  , InternalError(..)

  -- * Writing
  , WritableInto(..), ToChType(toChType)
  , insertInto

  -- * Arbitrary commands
  , command

  -- * Ping database connection
  , ping

  -- * ClickHouse types
  , IsChType(ToChTypeName, chTypeName, defaultValueOfTypeName)
  , DateTime(..)
  , Int8, Int16, Int32, Int64, Int128(..)
  , UInt8, UInt16, UInt32, UInt64, UInt128, Word128(..)
  , Nullable
  , LowCardinality, IsLowCardinalitySupported
  , UUID(..)
  , Array(..)
  , ChString(..)


  -- * Protocol parts
  -- ** Shared
  , UVarInt(..)
  {- *** Data packet -}, DataPacket(..), BlockInfo(..)

  -- ** Client
  {- *** Hello -}, HelloPacket(..), Addendum(..)
  {- *** Ping -}, PingPacket(..)
  {- *** Query -}
  , QueryPacket(..)
  , DbSettings(..), QueryParameters(..), QueryStage(..)
  , ClientInfo(..), QueryKind(..)
  
  -- ** Server
  {- *** Hello -}, HelloResponse(..), PasswordComplexityRules(..)
  {- *** Exception -}, ExceptionPacket(..)
  {- *** Progress -}, ProgressPacket(..)
  {- *** ProfileInfo -}, ProfileInfo(..)
  {- *** TableColumns -}, TableColumns(..)
  ) where

-- Internal
import Paths_ClickHaskell (version)

-- GHC included
import Control.Applicative (liftA2, liftA3)
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData)
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, mask, onException, throw, throwIO)
import Control.Monad (forM, replicateM, void, when, (<$!>), (<=<))
import Data.Binary.Get
import Data.Binary.Get.Internal (readN)
import Data.Bits (Bits (setBit, unsafeShiftL, unsafeShiftR, (.&.), (.|.)))
import Data.ByteString as BS (ByteString, length, take)
import Data.ByteString.Builder
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Lazy as BSL (toStrict)
import Data.ByteString.Char8 as BS8 (concatMap, length, pack, replicate, singleton)
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.List (uncons)
import Data.Maybe (listToMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding as Text (encodeUtf8)
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Data.Typeable (Proxy (..))
import Data.Version (Version (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (C1, D1, Generic (..), K1 (K1, unK1), M1 (M1, unM1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (AppendSymbol, ErrorMessage (..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

-- External
import Data.WideWord (Int128 (..), Word128(..))
import Network.Socket hiding (SocketOption(..))
import Network.Socket qualified as Sock (SocketOption(..))
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)
import GHC.Exts (inline, oneShot)

-- * Connection

data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }

defaultCredentials :: ChCredential
defaultCredentials = MkChCredential
  { chLogin    = "default"
  , chPass     = ""
  , chHost     = "localhost"
  , chDatabase = "default"
  , chPort     = "9000"
  }

data Connection where MkConnection :: (MVar ConnectionState) -> Connection

withConnection :: HasCallStack => Connection -> (ConnectionState -> IO a) -> IO a
withConnection (MkConnection connStateMVar) f =
  mask $ \restore -> do
    connState <- takeMVar connStateMVar
    b <- onException
      (restore (f connState))
      (putMVar connStateMVar =<< reopenConnection connState)
    putMVar connStateMVar connState
    return b

data ConnectionState = MkConnectionState
  { sock     :: Socket
  , user     :: ChString
  , hostname :: ChString
  , os_user  :: ChString
  , buffer   :: Buffer
  , revision :: ProtocolRevision
  , creds    :: ChCredential
  }

data ConnectionError = NoAdressResolved | EstablishTimeout
  deriving (Show, Exception)

writeToConnection :: Serializable packet => ConnectionState -> packet -> IO ()
writeToConnection MkConnectionState{sock, revision} packet =
  (sendAll sock . toLazyByteString . serialize revision) packet

writeToConnectionEncode :: ConnectionState -> (ProtocolRevision -> Builder) -> IO ()
writeToConnectionEncode MkConnectionState{sock, revision} serializer =
  (sendAll sock . toLazyByteString) (serializer revision)

openNativeConnection :: HasCallStack => ChCredential -> IO Connection
openNativeConnection creds = fmap MkConnection . newMVar =<< createConnectionState creds

reopenConnection :: ConnectionState -> IO ConnectionState
reopenConnection MkConnectionState{..} = do
  flushBuffer buffer
  close sock
  createConnectionState creds

createConnectionState :: ChCredential -> IO ConnectionState
createConnectionState creds@MkChCredential{chHost, chPort, chLogin, chPass, chDatabase} = do
  hostname <- maybe "" toChType <$> lookupEnv "HOSTNAME"
  os_user <- maybe "" toChType <$> lookupEnv "USER"
  AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress}
    <- maybe (throwIO NoAdressResolved) pure . listToMaybe
    =<< getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just chHost)
      (Just chPort)
  sock <- maybe (throwIO EstablishTimeout) pure
    =<< timeout 3_000_000 (
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
           setSocketOption sock Sock.NoDelay 1
           setSocketOption sock Sock.KeepAlive 1
           connect sock addrAddress
           pure sock
        )
      )

  (sendAll sock . toLazyByteString . serialize latestSupportedRevision)
    (mkHelloPacket MkHelloParameters{..})

  buffer <- initBuffer 4096 sock
  serverPacketType <- rawBufferizedRead buffer (deserialize latestSupportedRevision)
  case serverPacketType of
    HelloResponse MkHelloResponse{server_revision} -> do
      let revision = min server_revision latestSupportedRevision
          conn = MkConnectionState{user = toChType chLogin, ..}
      writeToConnection conn mkAddendum
      pure conn
    Exception exception -> throwIO (UserError $ DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


{- |
  Arbitrary commands wrapper

  For example: `CREATE`
-}
command :: HasCallStack => Connection -> ChString -> IO ()
command conn query = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState query)
    writeToConnection connState (mkDataPacket "" 0 0)
    handleCreate connState
  where
  handleCreate :: ConnectionState -> IO ()
  handleCreate MkConnectionState{..} =
    rawBufferizedRead buffer (deserialize revision)
    >>= \packet -> case packet of
      EndOfStream         -> pure ()
      Exception exception -> throwIO (UserError $ DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


-- * Ping

ping :: HasCallStack => Connection -> IO ()
ping conn = do
  withConnection conn $ \connState@MkConnectionState{revision, buffer} -> do
    writeToConnection connState mkPingPacket
    responsePacket <- rawBufferizedRead buffer (deserialize revision)
    case responsePacket of
      Pong                -> pure ()
      Exception exception -> throwIO (UserError $ DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)




-- * Reading

select ::
  forall columns record result
  .
  (ReadableFrom (Columns columns) record)
  =>
  Connection -> ChString -> ([record] -> IO result) -> IO [result]
select conn query f = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState query)
    writeToConnection connState (mkDataPacket "" 0 0)
    handleSelect @(Columns columns) connState (\x -> id <$!> f x)

data Table (name :: Symbol) (columns :: [Type])

selectFrom ::
  forall table record name columns a
  .
  ( table ~ Table name columns
  , KnownSymbol name
  , ReadableFrom table record
  )
  =>
  Connection -> ([record] -> IO a) -> IO [a]
selectFrom conn f = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState (toChType query))
    writeToConnection connState (mkDataPacket "" 0 0)
    handleSelect @table connState (\x -> id <$!> f x)
  where
    query =
      "SELECT " <> readingColumns @table @record <>
      " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)

data View (name :: Symbol) (columns :: [Type]) (parameters :: [Type])

selectFromView ::
  forall view record result name columns parameters passedParameters
  .
  ( ReadableFrom view record
  , KnownSymbol name
  , view ~ View name columns parameters
  , CheckParameters parameters passedParameters
  )
  => Connection -> (Parameters '[] -> Parameters passedParameters) -> ([record] -> IO result) -> IO [result]
selectFromView conn interpreter f = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState (toChType query))
    writeToConnection connState (mkDataPacket "" 0 0)
    handleSelect @view connState (\x -> id <$!> f x)
    where
    query =
      "SELECT " <> readingColumns @view @record <>
      " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> viewParameters interpreter

generateRandom ::
  forall columns record result
  .
  (ReadableFrom (Columns columns) record)
  =>
  Connection -> (UInt64, UInt64, UInt64) -> UInt64 -> ([record] -> IO result) -> IO [result]
generateRandom conn (randomSeed, maxStrLen, maxArrayLen) limit f = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState query)
    writeToConnection connState (mkDataPacket "" 0 0)
    handleSelect @(Columns columns) connState (\x -> id <$!> f x)
  where
  query =
    let columns = readingColumnsAndTypes @(Columns columns) @record
    in toChType $
      "SELECT * FROM generateRandom(" <>
          "'" <> columns <> "' ," <>
            toQueryPart randomSeed <> "," <>
            toQueryPart maxStrLen <> "," <>
            toQueryPart maxArrayLen <>
        ")" <>
      " LIMIT " <> toQueryPart limit <> ";"

-- *** Internal

handleSelect ::
  forall hasColumns record result
  .
  ReadableFrom hasColumns record
  =>
  ConnectionState -> ([record] -> IO result) -> IO [result]
handleSelect MkConnectionState{..} f = loop []
  where
  loop acc = rawBufferizedRead buffer (deserialize revision) >>=
    \packet -> case packet of
      DataResponse MkDataPacket{columns_count = 0, rows_count = 0} -> loop acc
      DataResponse MkDataPacket{rows_count} -> do
        result <- f =<< rawBufferizedRead buffer (deserializeColumns @hasColumns revision rows_count)
        loop (result : acc)
      Progress    _       -> loop acc
      ProfileInfo _       -> loop acc
      EndOfStream         -> pure acc
      Exception exception -> throwIO (UserError $ DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)




-- * Writing

insertInto ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , WritableInto table record
  , KnownSymbol name
  )
  => Connection -> [record] -> IO ()
insertInto conn columnsData = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState query)
    writeToConnection connState (mkDataPacket "" 0 0)
    handleInsertResult @table connState columnsData
  where
  query = toChType $
    "INSERT INTO " <> (byteString . BS8.pack . symbolVal @name $ Proxy)
    <> " (" <> writingColumns @table @record <> ") VALUES"
    

handleInsertResult :: forall columns record . WritableInto columns record => ConnectionState -> [record] -> IO ()
handleInsertResult conn@MkConnectionState{..} records = do
  firstPacket <- rawBufferizedRead buffer (deserialize revision)
  case firstPacket of
    TableColumns      _ -> handleInsertResult @columns conn records
    DataResponse MkDataPacket{} -> do
      _emptyDataPacket <- rawBufferizedRead buffer (deserializeInsertHeader @columns @record revision)
      writeToConnection conn (mkDataPacket "" (columnsCount @columns @record) (fromIntegral $ Prelude.length records))
      writeToConnectionEncode conn (serializeRecords @columns records)
      writeToConnection conn (mkDataPacket "" 0 0)
      handleInsertResult @columns @record conn []
    EndOfStream         -> pure ()
    Exception exception -> throwIO (UserError $ DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)




-- * Bufferization

data Buffer = MkBuffer
  { bufferSize :: Int
  , bufferSocket :: Socket
  , buff :: IORef BS.ByteString
  }

initBuffer :: Int -> Socket -> IO Buffer
initBuffer size sock = MkBuffer size sock <$> newIORef ""

flushBuffer :: Buffer -> IO ()
flushBuffer MkBuffer{buff} = atomicWriteIORef buff ""

readBuffer ::  Buffer -> IO BS.ByteString
readBuffer buffer@MkBuffer{..} =
  readIORef buff
    >>= (\currentBuffer ->
      case BS.length currentBuffer of
        0 -> recv bufferSocket bufferSize
        _ -> flushBuffer buffer *> pure currentBuffer
    )

writeToBuffer :: Buffer -> BS.ByteString -> IO ()
writeToBuffer MkBuffer{..} val = void (atomicModifyIORef buff (val,))

rawBufferizedRead :: Buffer -> Get packet -> IO packet
rawBufferizedRead buffer parser = runBufferReader buffer (runGetIncremental parser)

runBufferReader :: Buffer -> Decoder packet -> IO packet
runBufferReader buffer = \case
  (Partial decoder) -> readBuffer buffer >>= runBufferReader buffer . decoder . Just
  (Done leftover _consumed packet) -> packet <$ writeToBuffer buffer leftover
  (Fail _leftover _consumed msg) -> throwIO (InternalError $ DeserializationError msg)




-- * Errors handling

data ClientError where
  UserError :: HasCallStack => UserError -> ClientError
  InternalError :: HasCallStack => InternalError -> ClientError

instance Show ClientError where
  show (UserError err)         = "UserError " <> show err <> "\n" <> prettyCallStack callStack
  show (InternalError err)     = "InternalError " <> show err <> "\n" <> prettyCallStack callStack

deriving anyclass instance Exception ClientError

{- |
  You shouldn't see this exceptions. Please report a bug if it appears
-}
data InternalError
  = UnexpectedPacketType UVarInt
  | DeserializationError String
  deriving (Show, Exception)

data UserError
  = UnmatchedType String
  | UnmatchedColumn String
  | DatabaseException ExceptionPacket
  deriving (Show, Exception)




-- * Client packets

data ClientPacketType
  = Hello | Query | Data | Cancel | Ping | TablesStatusRequest
  | KeepAlive | Scalar | IgnoredPartUUIDs | ReadTaskResponse
  | MergeTreeReadTaskResponse | SSHChallengeRequest | SSHChallengeResponse
  deriving (Enum)

type family PacketTypeNumber (packetType :: ClientPacketType)
  where
  PacketTypeNumber Hello                     = 0; PacketTypeNumber Query = 1
  PacketTypeNumber Data                      = 2; PacketTypeNumber Cancel = 3
  PacketTypeNumber Ping                      = 4; PacketTypeNumber TablesStatusRequest = 5
  PacketTypeNumber KeepAlive                 = 6; PacketTypeNumber Scalar = 7
  PacketTypeNumber IgnoredPartUUIDs          = 8; PacketTypeNumber ReadTaskResponse = 9
  PacketTypeNumber MergeTreeReadTaskResponse = 10; PacketTypeNumber SSHChallengeRequest = 11
  PacketTypeNumber SSHChallengeResponse      = 12

data Packet (packetType :: ClientPacketType) = MkPacket

packetNumVal :: forall packetType . KnownNat (PacketTypeNumber packetType) => UVarInt
packetNumVal = fromIntegral . natVal $ Proxy @(PacketTypeNumber packetType)

instance
  KnownNat (PacketTypeNumber packetType)
  =>
  Serializable (Packet (packetType :: ClientPacketType)) where
  serialize rev _ = serialize @UVarInt rev (packetNumVal @packetType)

instance Deserializable (Packet (packetType :: ClientPacketType)) where
  deserialize _rev = pure (MkPacket @packetType)


-- ** Hello

data HelloParameters = MkHelloParameters
  { chDatabase :: Text
  , chLogin :: Text
  , chPass :: Text
  }

mkHelloPacket :: HelloParameters -> HelloPacket
mkHelloPacket MkHelloParameters{chDatabase, chLogin, chPass} =
  MkHelloPacket
    { packet_type          = MkPacket
    , client_name          = clientName
    , client_version_major = major
    , client_version_minor = minor
    , tcp_protocol_version = latestSupportedRevision
    , default_database     = toChType chDatabase
    , user                 = toChType chLogin
    , password             = toChType chPass
    }

data HelloPacket = MkHelloPacket
  { packet_type          :: Packet Hello
  , client_name          :: ChString
  , client_version_major :: UVarInt
  , client_version_minor :: UVarInt
  , tcp_protocol_version :: ProtocolRevision
  , default_database     :: ChString
  , user                 :: ChString
  , password             :: ChString
  }
  deriving (Generic, Serializable)


data Addendum = MkAddendum{quota_key :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY}
  deriving (Generic, Serializable)
mkAddendum :: Addendum
mkAddendum = MkAddendum{quota_key = MkSinceRevision ""}


-- ** Ping

data PingPacket = MkPingPacket{packet_type :: Packet Ping}
  deriving (Generic, Serializable)
mkPingPacket :: PingPacket
mkPingPacket = MkPingPacket{packet_type = MkPacket}


-- ** Query

mkQueryPacket :: ConnectionState -> ChString -> QueryPacket
mkQueryPacket MkConnectionState{..} query = MkQueryPacket
  { query_packet = MkPacket
  , query_id = ""
  , client_info                    = MkSinceRevision MkClientInfo
    { query_kind                   = InitialQuery
    , initial_user                 = user
    , initial_query_id             = ""
    , initial_adress               = "0.0.0.0:0"
    , initial_time                 = MkSinceRevision 0
    , interface_type               = 1 -- [tcp - 1, http - 2]
    , os_user, hostname
    , client_name                  = clientName
    , client_version_major         = major
    , client_version_minor         = minor
    , client_revision              = revision
    , quota_key                    = MkSinceRevision ""
    , distrubuted_depth            = MkSinceRevision 0
    , client_version_patch         = MkSinceRevision patch
    , open_telemetry               = MkSinceRevision 0
    , collaborate_with_initiator   = MkSinceRevision 0
    , count_participating_replicas = MkSinceRevision 0
    , number_of_current_replica    = MkSinceRevision 0
    }
  , settings           = MkDbSettings
  , interserver_secret = MkSinceRevision ""
  , query_stage        = Complete
  , compression        = 0
  , query
  , parameters         = MkSinceRevision MkQueryParameters
  }

data QueryPacket = MkQueryPacket
  { query_packet       :: Packet Query
  , query_id           :: ChString
  , client_info        :: ClientInfo `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_INFO
  , settings           :: DbSettings
  , interserver_secret :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
  , query_stage        :: QueryStage
  , compression        :: UVarInt
  , query              :: ChString
  , parameters         :: QueryParameters `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
  }
  deriving (Generic, Serializable)

data DbSettings = MkDbSettings
instance Serializable DbSettings where serialize rev _ = serialize @ChString rev ""

data QueryParameters = MkQueryParameters
instance Serializable QueryParameters where serialize rev _ = serialize @ChString rev ""

data QueryStage
  = FetchColumns | WithMergeableState | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

instance Serializable QueryStage where
  serialize rev = serialize @UVarInt rev . fromIntegral . fromEnum


data Flags = IMPORTANT | CUSTOM | OBSOLETE
_flagCode :: Flags -> UInt64
_flagCode IMPORTANT = 0x01
_flagCode CUSTOM    = 0x02
_flagCode OBSOLETE  = 0x04

data ClientInfo = MkClientInfo
  { query_kind                   :: QueryKind
  , initial_user                 :: ChString
  , initial_query_id             :: ChString
  , initial_adress               :: ChString
  , initial_time                 :: Int64 `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME
  , interface_type               :: UInt8
  , os_user                      :: ChString
  , hostname                     :: ChString
  , client_name                  :: ChString
  , client_version_major         :: UVarInt
  , client_version_minor         :: UVarInt
  , client_revision              :: ProtocolRevision
  , quota_key                    :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
  , distrubuted_depth            :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
  , client_version_patch         :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , open_telemetry               :: UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_OPENTELEMETRY
  , collaborate_with_initiator   :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , count_participating_replicas :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , number_of_current_replica    :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  }
  deriving (Generic, Serializable)

data QueryKind = NoQuery | InitialQuery | SecondaryQuery
instance Serializable QueryKind where
  serialize rev = serialize @UInt8 rev . (\case NoQuery -> 1; InitialQuery -> 2; SecondaryQuery -> 3)

-- ** Data

mkDataPacket :: ChString -> UVarInt -> UVarInt -> DataPacket
mkDataPacket table_name columns_count rows_count =
  MkDataPacket
    { packet_type   = MkPacket
    , table_name
    , block_info    = MkBlockInfo
      { field_num1   = 1, is_overflows = 0
      , field_num2   = 2, bucket_num   = -1
      , eof          = 0
      }
    , columns_count
    , rows_count
    }

data DataPacket = MkDataPacket
  { packet_type   :: Packet Data
  , table_name    :: ChString
  , block_info    :: BlockInfo
  , columns_count :: UVarInt
  , rows_count    :: UVarInt
  }
  deriving (Generic, Serializable, Deserializable)

data BlockInfo = MkBlockInfo
  { field_num1   :: UVarInt, is_overflows :: UInt8
  , field_num2   :: UVarInt, bucket_num   :: Int32
  , eof          :: UVarInt
  }
  deriving (Generic, Serializable, Deserializable)




-- * Server packets

data ServerPacketType where
  HelloResponse :: HelloResponse -> ServerPacketType
  DataResponse :: DataPacket -> ServerPacketType
  Exception :: ExceptionPacket -> ServerPacketType
  Progress :: ProgressPacket -> ServerPacketType
  Pong :: ServerPacketType
  EndOfStream :: ServerPacketType
  ProfileInfo :: ProfileInfo -> ServerPacketType
  Totals :: ServerPacketType
  Extremes :: ServerPacketType
  TablesStatusResponse :: ServerPacketType
  Log :: ServerPacketType
  TableColumns :: TableColumns -> ServerPacketType
  UUIDs :: ServerPacketType
  ReadTaskRequest :: ServerPacketType
  ProfileEvents :: ServerPacketType
  UnknownPacket :: UVarInt -> ServerPacketType

instance Deserializable ServerPacketType where
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
      14 -> pure ProfileEvents
      _  -> pure $ UnknownPacket packetNum

serverPacketToNum :: ServerPacketType -> UVarInt
serverPacketToNum = \case
  (HelloResponse _) -> 0; (DataResponse _)       -> 1
  (Exception _)     -> 2; (Progress _)           -> 3;
  (Pong)            -> 4; (EndOfStream)          -> 5
  (ProfileInfo _)   -> 6; (Totals)               -> 7
  (Extremes)        -> 8; (TablesStatusResponse) -> 9
  (Log)             -> 10; (TableColumns _)      -> 11;
  (UUIDs)           -> 12; (ReadTaskRequest)     -> 13
  (ProfileEvents)   -> 14; (UnknownPacket num)   -> num


-- ** HelloResponse

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
  }
  deriving (Generic, Deserializable)

data PasswordComplexityRules = MkPasswordComplexityRules
  { original_pattern  :: ChString
  , exception_message :: ChString
  }
  deriving (Generic, Deserializable)

instance Deserializable [PasswordComplexityRules] where
  deserialize rev = do
    len <- deserialize @UVarInt rev
    replicateM (fromIntegral len) (deserialize @PasswordComplexityRules rev)

-- ** Exception

data ExceptionPacket = MkExceptionPacket
  { code        :: Int32
  , name        :: ChString
  , message     :: ChString
  , stack_trace :: ChString
  , nested      :: UInt8
  }
  deriving (Generic, Show, Deserializable)

-- ** Progress

data ProgressPacket = MkProgressPacket
  { rows        :: UVarInt
  , bytes       :: UVarInt
  , total_rows  :: UVarInt
  , total_bytes :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_rows  :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_bytes :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  , elapsed_ns  :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  }
  deriving (Generic, Deserializable)

-- ** ProfileInfo

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
  deriving (Generic, Deserializable)

-- ** TableColumns

data TableColumns = MkTableColumns
  { table_name :: ChString
  , table_columns :: ChString
  }
  deriving (Generic, Deserializable)








-- * Deserialization

-- ** Generic API

type GenericReadable record hasColumns =
  ( Generic record
  , GReadable (GetColumns hasColumns) (Rep record)
  )

class HasColumns hasColumns => ReadableFrom hasColumns record
  where
  default deserializeColumns :: GenericReadable record hasColumns => ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns :: ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns rev size = do
    list <- gFromColumns @(GetColumns hasColumns) rev size
    pure $ do
      element <- list
      case to element of res -> pure $! res

  default readingColumns :: GenericReadable record hasColumns => Builder
  readingColumns :: Builder
  readingColumns = buildCols (gReadingColumns @(GetColumns hasColumns) @(Rep record))
    where
    buildCols [] = mempty
    buildCols ((col, _):[])   = col
    buildCols ((col, _):rest) = col <> ", " <> buildCols rest

  default readingColumnsAndTypes :: GenericReadable record hasColumns => Builder
  readingColumnsAndTypes ::  Builder
  readingColumnsAndTypes = buildColsTypes (gReadingColumns @(GetColumns hasColumns) @(Rep record))
    where
    buildColsTypes [] = mempty
    buildColsTypes ((col, typ):[])   = col <> " " <> typ
    buildColsTypes ((col, typ):rest) = col <> " " <> typ <> ", " <> buildColsTypes rest


class GReadable (columns :: [Type]) f
  where
  gFromColumns :: ProtocolRevision -> UVarInt -> Get [f p]
  gReadingColumns :: [(Builder, Builder)]

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = map (M1 . M1) <$> gFromColumns @columns rev size
  gReadingColumns = gReadingColumns @columns @f

instance
  (GReadable columns left, GReadable columns right1,  GReadable columns right2)
  =>
  GReadable columns ((left :*: right1) :*: right2)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size =
    liftA3
      (zipWith3 (\a b c -> (a :*: b) :*: c))
      (gFromColumns @columns @left rev size)
      (gFromColumns @columns @right1 rev size)
      (gFromColumns @columns @right2 rev size)

  gReadingColumns = gReadingColumns @columns @left ++ gReadingColumns @columns @right1 ++ gReadingColumns @columns @right2

instance
  ( KnownColumn (Column name chType)
  , GReadable '[Column name chType] (S1 (MetaSel (Just name) a b f) rec)
  , GReadable restColumns right
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  )
  =>
  GReadable columns (S1 (MetaSel (Just name) a b f) rec :*: right)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = do
    liftA2
      (zipWith (:*:))
      (gFromColumns @'[Column name chType] rev size)
      (gFromColumns @restColumns rev size)
  gReadingColumns =
    (renderColumnName @(Column name chType), renderColumnType @(Column name chType))
    : gReadingColumns @restColumns @right

instance
  ( KnownColumn (Column name chType)
  , DeserializableColumn (Column name chType)
  , FromChType chType inputType
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  ) => GReadable columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size =
    map (M1 . K1 . fromChType @chType) . columnValues
      <$> deserializeColumn @(Column name chType) rev True size
  gReadingColumns = (renderColumnName @(Column name chType), renderColumnType @(Column name chType)) : []


-- ** Column deserialization

{-# SPECIALIZE replicateM :: Int -> Get chType -> Get [chType] #-}

class DeserializableColumn column where
  deserializeColumn :: ProtocolRevision -> Bool -> UVarInt -> Get column

handleColumnHeader :: forall column . KnownColumn column => ProtocolRevision -> Bool -> Get ()
handleColumnHeader rev isCheckRequired = do
  let expectedColumnName = toChType (renderColumnName @column)
  resultColumnName <- deserialize @ChString rev 
  when (isCheckRequired && resultColumnName /= expectedColumnName)
    . throw . UserError . UnmatchedColumn
      $ "Got column \"" <> show resultColumnName <> "\" but expected \"" <> show expectedColumnName <> "\""

  let expectedType = toChType (renderColumnType @column)
  resultType <- deserialize @ChString rev
  when (isCheckRequired && resultType /= expectedType)
    . throw . UserError . UnmatchedType
      $  "Column " <> show resultColumnName <> " has type " <> show resultType <> ". But expected type is " <> show expectedType

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name chType) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired rows = do
    handleColumnHeader @(Column name chType) rev isCheckRequired
    _isCustom <- deserialize @(UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    column <- replicateM (fromIntegral rows) (deserialize @chType rev)
    pure $ mkColumn @(Column name chType) column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name (Nullable chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired rows = do
    handleColumnHeader @(Column name (Nullable chType)) rev isCheckRequired
    _isCustom <- deserialize @(UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    nulls <- replicateM (fromIntegral rows) (deserialize @UInt8 rev)
    nullable <-
      forM
        nulls
        (\case
          0 -> Just <$> deserialize @chType rev
          _ -> (Nothing <$ deserialize @chType rev)
        )
    pure $ mkColumn @(Column name (Nullable chType)) nullable

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (LowCardinality chType))
  , Deserializable chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  DeserializableColumn (Column name (LowCardinality chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired rows = do
    handleColumnHeader @(Column name (LowCardinality chType)) rev isCheckRequired
    _isCustom <- deserialize @(UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    _serializationType <- (.&. 0xf) <$> deserialize @UInt64 rev
    _index_size <- deserialize @Int64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    lc <- replicateM (fromIntegral rows) (toChType <$> deserialize @chType rev)
    pure $ mkColumn @(Column name (LowCardinality chType)) lc

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Array chType))
  , Deserializable chType
  , TypeError ('Text "Arrays deserialization still unsupported")
  )
  => DeserializableColumn (Column name (Array chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired _rows = do
    handleColumnHeader @(Column name (Array chType)) rev isCheckRequired
    _isCustom <- deserialize @(UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    (arraySize, _offsets) <- readOffsets rev
    _types <- replicateM (fromIntegral arraySize) (deserialize @chType rev)
    pure $ mkColumn @(Column name (Array chType)) []
    where
    readOffsets :: ProtocolRevision -> Get (UInt64, [UInt64])
    readOffsets revivion = do
      size <- deserialize @UInt64 rev
      (size, ) <$> go size
      where
      go arraySize =
        do
        nextOffset <- deserialize @UInt64 revivion
        if arraySize >= nextOffset
          then pure [nextOffset]
          else (nextOffset :) <$> go arraySize


-- ** Generics

class
  Deserializable chType
  where
  {-# INLINE deserialize #-}
  default deserialize :: (Generic chType, GDeserializable (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev

class GDeserializable f
  where
  gDeserialize :: ProtocolRevision -> Get (f p)

instance
  GDeserializable f
  =>
  GDeserializable (D1 c (C1 c2 f))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = M1 . M1 <$> gDeserialize rev

instance
  (GDeserializable left, GDeserializable right1, GDeserializable right2)
  =>
  GDeserializable ((left :*: right1) :*: right2)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = do
    l  <- gDeserialize rev
    r1 <- gDeserialize rev
    r2 <- gDeserialize rev
    pure ((l :*: r1) :*: r2)

instance
  (GDeserializable (S1 metaSel field), GDeserializable right)
  =>
  GDeserializable (S1 metaSel field :*: right)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = liftA2 (:*:) (gDeserialize rev)  (gDeserialize rev)

instance {-# OVERLAPPING #-}
  (GDeserializable right)
  =>
  (GDeserializable (S1 metaSel (Rec0 ProtocolRevision) :*: right))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = do
    chosenRev <- min rev . coerce <$> deserialize @UVarInt rev
    (:*:) (M1 $ K1 chosenRev) <$> gDeserialize @right chosenRev

instance
  Deserializable chType
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev =  M1 . K1 <$> deserialize @chType rev


-- ** Database types

instance Deserializable Int8 where deserialize _ = toChType <$> getInt8; {-# INLINE deserialize #-}
instance Deserializable Int16 where deserialize _ = toChType <$> getInt16le; {-# INLINE deserialize #-}
instance Deserializable Int32 where deserialize _ = toChType <$> getInt32le; {-# INLINE deserialize #-}
instance Deserializable Int64 where deserialize _ = toChType <$> getInt64le; {-# INLINE deserialize #-}
instance Deserializable Int128 where deserialize _ = toChType <$> liftA2 (flip Int128) getWord64le getWord64le; {-# INLINE deserialize #-}
instance Deserializable UInt8 where deserialize _ = toChType <$> getWord8; {-# INLINE deserialize #-}
instance Deserializable UInt16 where deserialize _ = toChType <$> getWord16le; {-# INLINE deserialize #-}
instance Deserializable UInt32 where deserialize _ = toChType <$> getWord32le; {-# INLINE deserialize #-}
instance Deserializable UInt64 where deserialize _ = toChType <$> getWord64le; {-# INLINE deserialize #-}
instance Deserializable UInt128 where deserialize _ = toChType <$> liftA2 (flip Word128) getWord64le getWord64le; {-# INLINE deserialize #-}
instance Deserializable UUID where deserialize _ = MkChUUID <$> liftA2 (flip Word128) getWord64le getWord64le; {-# INLINE deserialize #-}
instance Deserializable ChString where deserialize = (\n -> toChType <$> readN n (BS.take n)) . fromIntegral <=< deserialize @UVarInt; {-# INLINE deserialize #-}
instance Deserializable Date where deserialize _ = toChType <$> getWord16le; {-# INLINE deserialize #-}
instance Deserializable (DateTime tz) where deserialize _ = toChType <$> getWord32le; {-# INLINE deserialize #-}
instance Deserializable UVarInt where
  {-# INLINE deserialize #-}
  deserialize _ = go 0 (0 :: UVarInt)
    where
    go i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else go (i + 1) $! o'
    go _ _ = fail "input exceeds varuint size"

-- ** FromChType

class FromChType chType outputType where fromChType  :: chType -> outputType

instance FromChType UUID (Word64, Word64) where fromChType (MkChUUID (Word128 w64hi w64lo)) = (w64hi, w64lo)
instance {-# OVERLAPPABLE #-} (IsChType chType, chType ~ inputType) => FromChType chType inputType where fromChType = id
instance FromChType (DateTime tz) Word32     where fromChType = coerce
instance FromChType (DateTime tz) UTCTime    where fromChType (MkDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)
instance
  FromChType chType inputType
  =>
  FromChType (Nullable chType) (Nullable inputType)
  where
  fromChType = fmap (fromChType @chType)
instance FromChType chType (LowCardinality chType) where
  fromChType = MkLowCardinality
instance FromChType Date Word16 where fromChType = coerce
instance
  FromChType chType outputType
  =>
  FromChType (LowCardinality chType) outputType
  where
  fromChType (MkLowCardinality value) = fromChType value
instance FromChType ChString BS.ByteString where fromChType (MkChString string) = string
instance FromChType ChString Builder where fromChType (MkChString string) = byteString string
instance
  ( TypeError
    (     'Text "ChString to Text using FromChType convertion could cause exception"
    ':$$: 'Text "Decode ByteString manually if you are sure it's always can be decoded or replace it with ByteString"
    )
  ) =>
  FromChType ChString Text
  where
  fromChType = error "Unreachable"
instance FromChType chType inputType => FromChType (Array chType) [inputType]
  where
  fromChType (MkChArray values) = map fromChType values








-- * Columns

-- ** Columns extraction helper

class HasColumns hasColumns where type GetColumns hasColumns :: [Type]
instance HasColumns (Columns columns)          where type GetColumns (Columns columns) = columns
instance HasColumns (Table name columns)       where type GetColumns (Table _ columns) = columns
instance HasColumns (View name columns params) where type GetColumns (View _ columns _) = columns


-- ** Take column by name from list of columns

type family
  TakeColumn (name :: Symbol) (columns :: [Type]) :: (Type, [Type])
  where
  TakeColumn name columns = GoTakeColumn name columns '[]

type family
  GoTakeColumn name (columns :: [Type]) (acc :: [Type]) :: (Type, [Type])
  where
  GoTakeColumn name (Column name chType ': columns) acc = '(Column name chType, acc ++ columns)
  GoTakeColumn name (Column name1 chType ': columns) acc = (GoTakeColumn name columns (Column name1 chType ': acc))
  GoTakeColumn name '[]                 acc = TypeError
    (    'Text "There is no column \"" :<>: 'Text name :<>: 'Text "\" in table"
    :$$: 'Text "You can't use this field"
    )

type family
  (++) (list1 :: [Type]) (list2 :: [Type]) :: [Type]
  where
  (++) '[]            list = list
  (++) (head ': tail) list = tail ++ (head ': list)


data Columns (columns :: [Type]) where
  Empty :: Columns '[]
  AddColumn
    :: KnownColumn (Column name chType)
    => Column name chType
    -> Columns columns
    -> Columns (Column name chType ': columns)

{- |
Column declaration

For example:

@
type MyColumn = Column "myColumn" ChString
@
-}
data Column (name :: Symbol) (chType :: Type) where
  UInt8Column   :: [UInt8]   -> Column name UInt8;   Int8Column   :: [Int8]   -> Column name Int8
  UInt16Column  :: [UInt16]  -> Column name UInt16;  Int16Column  :: [Int16]  -> Column name Int16
  UInt32Column  :: [UInt32]  -> Column name UInt32;  Int32Column  :: [Int32]  -> Column name Int32
  UInt64Column  :: [UInt64]  -> Column name UInt64;  Int64Column  :: [Int64]  -> Column name Int64
  UInt128Column :: [UInt128] -> Column name UInt128; Int128Column :: [Int128] -> Column name Int128
  DateTimeColumn :: [DateTime tz] -> Column name (DateTime tz)
  DateColumn :: [Date] -> Column name Date
  UUIDColumn :: [UUID] -> Column name UUID
  StringColumn :: [ChString] -> Column name ChString
  ArrayColumn :: [Array chType] -> Column name (Array chType)
  NullableColumn :: [Nullable chType] -> Column name (Nullable chType)
  LowCardinalityColumn :: IsLowCardinalitySupported chType => [chType] -> Column name (LowCardinality chType)

type family GetColumnName column :: Symbol where GetColumnName (Column name columnType) = name
type family GetColumnType column :: Type   where GetColumnType (Column name columnType) = columnType

{-# INLINE [0] columnValues #-}
columnValues :: Column name chType -> [chType]
columnValues column = case column of
  (UInt8Column values) -> values; (UInt16Column values) -> values
  (UInt32Column values) -> values; (UInt64Column values) -> values
  (UInt128Column values) -> values; (Int8Column values) -> values
  (Int16Column values) -> values; (Int32Column values) -> values
  (Int64Column values) -> values; (Int128Column values) -> values
  (DateColumn values) -> values; (DateTimeColumn values) -> values
  (UUIDColumn values) -> values; (StringColumn values) -> values
  (ArrayColumn values) -> values; (NullableColumn values) ->  values
  (LowCardinalityColumn values) -> map fromChType values

class
  ( IsChType (GetColumnType column)
  , KnownSymbol (GetColumnName column)
  ) =>
  KnownColumn column where
  renderColumnName :: Builder
  renderColumnName = (stringUtf8 . symbolVal @(GetColumnName column)) Proxy

  renderColumnType :: Builder
  renderColumnType = chTypeName @(GetColumnType column)

  mkColumn :: [GetColumnType column] -> Column (GetColumnName column) (GetColumnType column)

instance KnownSymbol name => KnownColumn (Column name UInt8) where mkColumn = UInt8Column
instance KnownSymbol name => KnownColumn (Column name UInt16) where mkColumn = UInt16Column
instance KnownSymbol name => KnownColumn (Column name UInt32) where mkColumn = UInt32Column
instance KnownSymbol name => KnownColumn (Column name UInt64) where mkColumn = UInt64Column
instance KnownSymbol name => KnownColumn (Column name UInt128) where mkColumn = UInt128Column
instance KnownSymbol name => KnownColumn (Column name Int8)  where mkColumn = Int8Column
instance KnownSymbol name => KnownColumn (Column name Int16) where mkColumn = Int16Column
instance KnownSymbol name => KnownColumn (Column name Int32) where mkColumn = Int32Column
instance KnownSymbol name => KnownColumn (Column name Int64) where mkColumn = Int64Column
instance KnownSymbol name => KnownColumn (Column name Int128) where mkColumn = Int128Column
instance KnownSymbol name => KnownColumn (Column name Date) where mkColumn = DateColumn
instance
  ( KnownSymbol name
  , IsChType (DateTime tz)
  ) =>
  KnownColumn (Column name (DateTime tz)) where mkColumn = DateTimeColumn
instance KnownSymbol name => KnownColumn (Column name UUID) where mkColumn = UUIDColumn
instance
  ( KnownSymbol name
  , IsChType chType
  , IsChType (Nullable chType)
  ) =>
  KnownColumn (Column name (Nullable chType)) where mkColumn = NullableColumn
instance KnownSymbol name => KnownColumn (Column name ChString) where mkColumn = StringColumn
instance
  ( KnownSymbol name
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  ) =>
  KnownColumn (Column name (LowCardinality chType)) where mkColumn = LowCardinalityColumn . map fromChType
instance KnownSymbol name => KnownColumn (Column name (Array ChString)) where mkColumn = ArrayColumn


-- ** Columns

instance
  Serializable (Columns '[])
  where
  {-# INLINE serialize #-}
  serialize _rev Empty = ""

instance (Serializable (Columns columns), Serializable col)
  =>
  Serializable (Columns (col ': columns))
  where
  {-# INLINE serialize #-}
  serialize rev (AddColumn col columns) = serialize rev col <> serialize rev columns

instance (KnownColumn (Column name chType), Serializable chType)
  =>
  Serializable (Column name chType) where
  {-# INLINE serialize #-}
  serialize rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name (Nullable chType)) where
  {-# INLINE serialize #-}
  serialize rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    -- Nulls
    <> mconcat (Prelude.map (serialize @UInt8 rev . maybe 1 (const 0)) (columnValues column))
    -- Values
    <> mconcat (Prelude.map (serialize @chType rev . maybe defaultValueOfTypeName id) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , IsChType chType
  , Serializable chType
  , TypeError ('Text "LowCardinality serialization still unsupported")
  ) => Serializable (Column name (LowCardinality chType)) where
  {-# INLINE serialize #-}
  serialize rev (LowCardinalityColumn column)
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    <> undefined column








-- * Parameters

type family KnownParameter param
  where
  KnownParameter (Parameter name parType) = (KnownSymbol name, IsChType parType, ToQueryPart parType)

data Parameter (name :: Symbol) (chType :: Type) = MkParamater chType

data Parameters parameters where
  NoParameters :: Parameters '[]
  AddParameter
    :: KnownParameter (Parameter name chType)
    => Parameter name chType
    -> Parameters parameters
    -> Parameters (Parameter name chType ': parameters)

{- |
>>> viewParameters (parameter @"a3" @ChString ("a3Val" :: String) . parameter @"a2" @ChString ("a2Val" :: String))
"(a3='a3Val', a2='a2Val')"
-}
viewParameters :: (Parameters '[] -> Parameters passedParameters) -> Builder
viewParameters interpreter = "(" <> renderParameters (interpreter NoParameters) <> ")"

renderParameters :: Parameters params -> Builder
renderParameters NoParameters                      = ""
renderParameters (AddParameter param NoParameters) = renderParameter param
renderParameters (AddParameter param moreParams)   = renderParameter param <> ", " <> renderParameters moreParams


parameter
  :: forall name chType parameters userType
  . (ToChType chType userType, KnownParameter (Parameter name chType))
  => userType -> Parameters parameters -> Parameters (Parameter name chType ': parameters)
parameter val = AddParameter (MkParamater $ toChType val)

renderParameter :: forall name chType . KnownParameter (Parameter name chType) => Parameter name chType -> Builder
renderParameter (MkParamater chType) = (byteString . BS8.pack . symbolVal @name) Proxy <> "=" <> toQueryPart chType

type family CheckParameters (required :: [Type]) (passed :: [Type]) :: Constraint
  where
  CheckParameters required passed = GoCheckParameters required passed '[]

type family GoCheckParameters required passed acc :: Constraint
  where
  GoCheckParameters '[] '[] '[] = ()
  GoCheckParameters (Parameter name _ ': _) '[] '[] = TypeError ('Text "Missing parameter \"" :<>: 'Text name :<>: 'Text "\".")
  GoCheckParameters '[] (p ': _) _ = TypeError ('Text "More parameters passed than used in the view")
  GoCheckParameters '[] '[] (p ': _) = TypeError ('Text "More parameters passed than used in the view")
  GoCheckParameters (Parameter name1 _ ': ps) '[] (Parameter name2 _ ': ps') = TypeError ('Text "Missing  \"" :<>: 'Text name1 :<>: 'Text "\" in passed parameters")
  GoCheckParameters (p ': ps) '[] (p' ': ps') = GoCheckParameters (p ': ps) (p' ': ps') '[]
  GoCheckParameters (Parameter name1 _ ': ps) (Parameter name1 _ ': ps') acc = (GoCheckParameters ps ps' acc)
  GoCheckParameters (Parameter name1 chType1 ': ps) (Parameter name2 chType2 ': ps') acc
    = (GoCheckParameters (Parameter name1 chType1 ': ps) ps' (Parameter name2 chType2 ': acc))
    
class ToQueryPart chType where toQueryPart :: chType -> BS.Builder
instance ToQueryPart Int8 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart Int16 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart Int32 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart Int64 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart Int128 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart UInt8 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart UInt16 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart UInt32 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart UInt64 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart UInt128 where toQueryPart = BS.byteString . BS8.pack . show
instance ToQueryPart chType => ToQueryPart (Nullable chType)
  where
  toQueryPart = maybe "null" toQueryPart
instance ToQueryPart chType => ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType
instance ToQueryPart UUID where
  toQueryPart (MkChUUID (Word128 hi lo)) = mconcat
    ["'", p 3 hi, p 2 hi, "-", p 1 hi, "-", p 0 hi, "-", p 3 lo, "-", p 2 lo, p 1 lo, p 0 lo, "'"]
    where
    p :: Int -> Word64 -> Builder
    p shiftN word = word16HexFixed $ fromIntegral (word `unsafeShiftR` (shiftN*16))
instance ToQueryPart ChString where
  toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"
    where
    escapeQuery :: BS.ByteString -> Builder
    escapeQuery -- [ClickHaskell.DbTypes.ToDo.1]: Optimize
      = BS.byteString . BS8.concatMap (\case '\'' -> "\\\'"; '\\' -> "\\\\"; sym -> BS8.singleton sym;)
instance ToQueryPart (DateTime tz)
  where
  toQueryPart chDateTime = let time = BS8.pack . show . fromChType @(DateTime tz) @Word32 $ chDateTime
    in BS.byteString (BS8.replicate (10 - BS8.length time) '0' <> time)
instance (IsChType chType, ToQueryPart chType) => ToQueryPart (Array chType)
  where
  toQueryPart
    = (\x -> "[" <> x <> "]")
    . (maybe "" (uncurry (foldr (\a b -> a <> "," <> b))) . uncons
    . map (toQueryPart @chType)) . fromChType @(Array chType) @[chType]








-- * Serialization

-- *** Generic API

type GenericWritable record columns =
  ( Generic record
  , GWritable columns (Rep record)
  )

class
  ( HasColumns (Columns (GetColumns columns))
  , Serializable (Columns (GetColumns columns))
  ) =>
  WritableInto columns record
  where
  default deserializeInsertHeader :: GenericWritable record (GetColumns columns) => ProtocolRevision -> Get ()
  deserializeInsertHeader :: ProtocolRevision -> Get ()
  deserializeInsertHeader rev = gDeserializeInsertHeader @(GetColumns columns) @(Rep record) rev

  default serializeRecords :: GenericWritable record (GetColumns columns) => [record] -> ProtocolRevision -> Builder
  serializeRecords :: [record] -> ProtocolRevision -> Builder
  serializeRecords records rev = gSerializeRecords @(GetColumns columns) rev (map from records)

  default writingColumns :: GenericWritable record (GetColumns columns) => Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns columns) @(Rep record)

  default columnsCount :: GenericWritable record (GetColumns columns) => UVarInt
  columnsCount :: UVarInt
  columnsCount = gColumnsCount @(GetColumns columns) @(Rep record)

class GWritable (columns :: [Type]) f
  where
  gDeserializeInsertHeader :: ProtocolRevision -> Get ()
  gSerializeRecords :: ProtocolRevision -> [f p] -> Builder
  gWritingColumns :: Builder
  gColumnsCount :: UVarInt

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = gSerializeRecords @columns rev . map (unM1 . unM1)
  {-# INLINE gDeserializeInsertHeader #-}
  gDeserializeInsertHeader rev = gDeserializeInsertHeader @columns @f rev
  gWritingColumns = gWritingColumns @columns @f
  gColumnsCount = gColumnsCount @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = gSerializeRecords @columns rev . map (\((l1 :*: l2) :*: r) -> l1 :*: (l2 :*: r))
  {-# INLINE gDeserializeInsertHeader #-}
  gDeserializeInsertHeader rev = void $ gDeserializeInsertHeader @columns @(left1 :*: (left2 :*: right)) rev
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))
  gColumnsCount = gColumnsCount @columns @(left1 :*: (left2 :*: right))

instance
  ( GWritable '[Column name chType] (S1 (MetaSel (Just name) a b f) rec)
  , GWritable restColumns right
  , '(Column name chType, restColumns)~ TakeColumn name columns
  )
  =>
  GWritable columns (S1 (MetaSel (Just name) a b f) rec :*: right)
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs =
    (\(ls,rs) -> gSerializeRecords @'[Column name chType] rev ls <> gSerializeRecords @restColumns rev rs)
      (foldr (\(l :*: r) (accL, accR) -> (l:accL, r:accR)) ([], []) xs)
  {-# INLINE gDeserializeInsertHeader #-}
  gDeserializeInsertHeader rev = do
    gDeserializeInsertHeader @'[Column name chType] @(S1 (MetaSel (Just name) a b f) rec) rev
    gDeserializeInsertHeader @restColumns @right rev
  gWritingColumns =
    gWritingColumns @'[Column name chType] @(S1 (MetaSel (Just name) a b f) rec)
    <> ", " <> gWritingColumns @restColumns @right
  gColumnsCount = gColumnsCount @'[Column name chType] @(S1 (MetaSel (Just name) a b f) rec) + gColumnsCount @restColumns @right

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name chType)
  , ToChType chType inputType
  , Serializable (Column name chType)
  , DeserializableColumn (Column name chType)
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  ) =>
  GWritable columns (S1 (MetaSel (Just name) a b f) (Rec0 inputType))
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = serialize rev . mkColumn @(Column name chType) . map (toChType . unK1 . unM1)
  {-# INLINE gDeserializeInsertHeader #-}
  gDeserializeInsertHeader rev = void $ deserializeColumn @(Column name chType) rev False 0
  gWritingColumns = renderColumnName @(Column name chType)
  gColumnsCount = 1

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
      | otherwise = word8 (setBit (fromIntegral i) 7) <> go (i `unsafeShiftR` 7)
instance Serializable ChString where
  serialize rev str = (serialize @UVarInt rev . fromIntegral . BS.length . fromChType) str <> fromChType str
instance Serializable UUID where serialize _ = (\(hi, lo) -> word64LE lo <> word64LE hi) . fromChType
instance Serializable Int8 where serialize _ = int8 . fromChType
instance Serializable Int16 where serialize _ = int16LE . fromChType
instance Serializable Int32 where serialize _ = int32LE . fromChType
instance Serializable Int64 where serialize _ = int64LE . fromChType
instance Serializable Int128 where serialize _ = (\(Int128 hi lo) -> word64LE lo <> word64LE hi) . fromChType
instance Serializable UInt8 where serialize _ = word8 . fromChType
instance Serializable UInt16 where serialize _ = word16LE . fromChType
instance Serializable UInt32 where serialize _ = word32LE . fromChType
instance Serializable UInt64 where serialize _ = word64LE . fromChType
instance Serializable UInt128 where serialize _ = (\(Word128 hi lo) -> word64LE lo <> word64LE hi) . fromChType
instance Serializable (DateTime tz) where serialize _ = word32LE . fromChType
instance Serializable Date where serialize _ = word16LE . fromChType


-- ** Generics

class GSerializable f
  where
  gSerialize :: ProtocolRevision -> f p -> Builder

instance
  GSerializable f
  =>
  GSerializable (D1 c (C1 c2 f))
  where
  {-# INLINE gSerialize #-}
  gSerialize rev (M1 (M1 re)) = gSerialize rev re

instance
  GSerializable (left1 :*: (left2 :*: right))
  =>
  GSerializable ((left1 :*: left2) :*: right)
  where
  {-# INLINE gSerialize #-}
  gSerialize rev ((l1 :*: l2) :*: r) = gSerialize rev (l1 :*: (l2 :*: r))

instance
  Serializable chType
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  {-# INLINE gSerialize #-}
  gSerialize rev = serialize rev . unK1 . unM1

instance
  (Serializable chType, GSerializable right)
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType) :*: right)
  where
  {-# INLINE gSerialize #-}
  gSerialize rev (left :*: right)
    = (serialize rev . unK1 . unM1 $ left) <> gSerialize rev right

-- ** ToChType

class ToChType chType inputType    where toChType    :: inputType -> chType

instance {-# OVERLAPPABLE #-} (IsChType chType, chType ~ inputType) => ToChType chType inputType where toChType = id
instance ToChType Int64 Int where toChType = fromIntegral
instance ToChType UInt128 UInt64 where toChType = fromIntegral
instance ToChType ChString BS.ByteString where toChType = MkChString
instance ToChType ChString Builder       where toChType = MkChString . toStrict . toLazyByteString
instance ToChType ChString String        where toChType = MkChString . BS8.pack
instance ToChType ChString Text          where toChType = MkChString . Text.encodeUtf8
instance ToChType ChString Int           where toChType = MkChString . BS8.pack . show
instance
  ToChType inputType chType
  =>
  ToChType (Nullable inputType) (Nullable chType)
  where
  toChType = fmap (toChType @inputType @chType)
instance ToChType inputType chType => ToChType (LowCardinality inputType) chType where toChType = MkLowCardinality . toChType
instance ToChType UUID Word64 where toChType = MkChUUID . flip Word128 0
instance ToChType UUID (Word64, Word64) where toChType = MkChUUID . uncurry (flip Word128)
instance ToChType (DateTime tz) Word32     where toChType = MkDateTime
instance ToChType (DateTime tz) UTCTime    where toChType = MkDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType (DateTime tz) ZonedTime  where toChType = MkDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC
instance ToChType Date Word16 where toChType = MkChDate
instance ToChType chType inputType => ToChType (Array chType) [inputType]
  where
  toChType = MkChArray . map toChType








class
  KnownSymbol (ToChTypeName chType)
  =>
  IsChType chType
  where
  -- | Shows database original type name
  --
  -- @
  -- type ToChTypeName ChString = \"String\"
  -- type ToChTypeName (Nullable UInt32) = \"Nullable(UInt32)\"
  -- @
  type ToChTypeName chType :: Symbol

  chTypeName :: Builder
  chTypeName = byteString . BS8.pack . symbolVal @(ToChTypeName chType) $ Proxy

  defaultValueOfTypeName :: chType

instance IsChType Int8 where
  type ToChTypeName Int8 = "Int8"
  defaultValueOfTypeName = 0

instance IsChType Int16 where
  type ToChTypeName Int16 = "Int16"
  defaultValueOfTypeName = 0

instance IsChType Int32 where
  type ToChTypeName Int32 = "Int32"
  defaultValueOfTypeName = 0

instance IsChType Int64 where
  type ToChTypeName Int64 = "Int64"
  defaultValueOfTypeName = 0

instance IsChType Int128 where
  type ToChTypeName Int128 = "Int128"
  defaultValueOfTypeName = 0
-- | ClickHouse UInt8 column type
type UInt8 = Word8
instance IsChType UInt8 where
  type ToChTypeName UInt8 = "UInt8"
  defaultValueOfTypeName = 0

-- | ClickHouse UInt16 column type
type UInt16 = Word16
instance IsChType UInt16 where
  type ToChTypeName UInt16 = "UInt16"
  defaultValueOfTypeName = 0

-- | ClickHouse UInt32 column type
type UInt32 = Word32
instance IsChType UInt32 where
  type ToChTypeName UInt32 = "UInt32"
  defaultValueOfTypeName = 0

-- | ClickHouse UInt64 column type
type UInt64 = Word64
instance IsChType UInt64 where
  type ToChTypeName UInt64 = "UInt64"
  defaultValueOfTypeName = 0

-- | ClickHouse UInt128 column type
type UInt128 = Word128
instance IsChType UInt128 where
  type ToChTypeName UInt128 = "UInt128"
  defaultValueOfTypeName = 0

-- | ClickHouse UUID column type
newtype UUID = MkChUUID Word128
  deriving newtype (Generic, Show, Eq, NFData, Bounded, Enum)
instance IsChType UUID where
  type ToChTypeName UUID = "UUID"
  defaultValueOfTypeName = MkChUUID 0

{- |
ClickHouse DateTime column type (paramtrized with timezone)

>>> chTypeName @(DateTime "")
"DateTime"
>>> chTypeName @(DateTime "UTC")
"DateTime('UTC')"
-}
newtype DateTime (tz :: Symbol) = MkDateTime Word32
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance KnownSymbol (ToChTypeName (DateTime tz)) => IsChType (DateTime tz)
  where
  type ToChTypeName (DateTime tz) = If (tz == "") ("DateTime") ("DateTime('" `AppendSymbol` tz `AppendSymbol` "')")
  defaultValueOfTypeName = MkDateTime 0

-- | ClickHouse String column type
newtype ChString = MkChString BS.ByteString
  deriving newtype (Show, Eq, IsString, NFData)
instance IsChType ChString where
  type ToChTypeName ChString = "String"
  defaultValueOfTypeName = ""

-- | ClickHouse Date column type
newtype Date = MkChDate Word16
  deriving newtype (Show, Eq, Bits, Bounded, Enum, NFData)
instance IsChType Date where
  type ToChTypeName Date = "Date"
  defaultValueOfTypeName = MkChDate 0

-- | ClickHouse Array column type
newtype Array a = MkChArray [a]
  deriving newtype (Show, Eq, NFData)
instance
  KnownSymbol (AppendSymbol (AppendSymbol "Array(" (ToChTypeName chType)) ")")
  =>
  IsChType (Array chType)
  where
  type ToChTypeName (Array chType) = "Array(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  defaultValueOfTypeName = MkChArray []


-- | ClickHouse Nullable(T) column type
-- (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

instance
  ( IsChType chType
  , KnownSymbol ("Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")")
  )
  =>
  IsChType (Nullable chType)
  where
  type ToChTypeName (Nullable chType) = NullableTypeName chType
  defaultValueOfTypeName = Nothing


-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
instance
  ( IsLowCardinalitySupported chType
  , KnownSymbol ("LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")")
  ) =>
  IsChType (LowCardinality chType)
  where
  type ToChTypeName (LowCardinality chType) = "LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  defaultValueOfTypeName = MkLowCardinality $ defaultValueOfTypeName @chType

deriving newtype instance (Eq chType, IsLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving newtype instance (NFData chType, IsLowCardinalitySupported chType) => NFData (LowCardinality chType)
deriving newtype instance IsString (LowCardinality ChString)

class IsChType chType => IsLowCardinalitySupported chType
instance IsLowCardinalitySupported ChString
instance
  ( IsLowCardinalitySupported chType
  , IsChType (Nullable chType)
  ) =>
  IsLowCardinalitySupported (Nullable chType)

instance {-# OVERLAPPABLE #-}
  ( IsChType chType
  , TypeError
    (    'Text "LowCardinality("  ':<>: 'ShowType chType  ':<>: 'Text ") is unsupported"
    ':$$: 'Text "Use one of these types:"
    ':$$: 'Text "  ChString"
    ':$$: 'Text "  DateTime"
    ':$$: 'Text "  Nullable(T)"
    )
  ) => IsLowCardinalitySupported chType




{- |
  Unsigned variable-length quantity encoding
  
  Part of protocol implementation
-}
newtype UVarInt = MkUVarInt Word64
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)








-- * Versioning

major, minor, patch :: UVarInt
major = case versionBranch version of (x:_) -> fromIntegral x; _ -> 0
minor = case versionBranch version of (_:x:_) -> fromIntegral x; _ -> 0
patch = case versionBranch version of (_:_:x:_) -> fromIntegral x; _ -> 0

clientName :: ChString
clientName = fromString $
  "ClickHaskell-" <> show major <> "." <> show minor <> "." <> show patch

newtype ProtocolRevision = MkProtocolRevision Word64
  deriving newtype (Eq, Num, Ord)

instance Serializable ProtocolRevision where serialize rev = serialize @UVarInt rev . coerce

{-# INLINE [0] afterRevision #-}
afterRevision
  :: forall revision monoid
  .  (KnownNat revision, Monoid monoid)
  => ProtocolRevision -> monoid -> monoid
afterRevision chosenRevision monoid =
  if chosenRevision >= (fromIntegral . natVal) (Proxy @revision)
  then monoid
  else mempty

latestSupportedRevision :: ProtocolRevision
latestSupportedRevision = (fromIntegral . natVal) (Proxy @DBMS_TCP_PROTOCOL_VERSION)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented
instance Show a => Show (SinceRevision a revisionNumber) where
  show (MkSinceRevision a) = show a
  show NotPresented = ""

instance
  (KnownNat revision, Deserializable chType)
  =>
  Deserializable (SinceRevision chType revision)
  where
  deserialize rev =
    if rev >= (fromIntegral . natVal) (Proxy @revision)
    then MkSinceRevision <$> deserialize @chType rev
    else pure NotPresented

instance
  (KnownNat revision, Serializable chType)
  =>
  Serializable (SinceRevision chType revision)
  where
  serialize rev (MkSinceRevision val) = afterRevision @revision rev (serialize rev val)
  serialize rev NotPresented          = afterRevision @revision rev (error "Unexpected error")


{-
  Slightly modified C++ sources:
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Core/ProtocolDefines.h#L6
-}
type DBMS_TCP_PROTOCOL_VERSION = 54448;

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
