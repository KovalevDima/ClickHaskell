{-# LANGUAGE
    BangPatterns
  , ConstraintKinds
  , DuplicateRecordFields
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , NoFieldSelectors
  , NumericUnderscores
  , OverloadedStrings
  , RecordWildCards
  , TupleSections
#-}

{-# OPTIONS_GHC
  -Wno-orphans
#-}

module ClickHaskell
  (
  -- * Connection
    ChCredential(..), defaultCredentials
  , Connection(..), openNativeConnection

  -- * Reading and writing
  , Table
  , Columns, Column, KnownColumn(..), DeserializableColumn(..)

  -- ** Reading
  , ReadableFrom(..)
  -- *** Simple
  , select
  , selectFrom
  , selectFromView, View, parameter, Parameter, Parameters, viewParameters
  -- *** Streaming
  , streamSelect
  , streamSelectFrom
  , streamSelectFromView
  -- *** Internal
  , handleSelect

  -- * Errors
  , ClientError(..)
  , ConnectionError(..)
  , UserError(..)
  , InternalError(..)

  -- ** Writing
  , WritableInto(..)
  , insertInto

  -- * Ping database connection
  , ping

  -- * ClickHouse types
  , IsChType(ToChTypeName, chTypeName, defaultValueOfTypeName)
  , ToChType(toChType)
  , FromChType(fromChType)
  , ToQueryPart(toQueryPart)

  , ChDateTime(..)
  , ChDate(..)

  , ChInt8(..), ChInt16(..), ChInt32(..), ChInt64(..), ChInt128(..)
  , ChUInt8(..), ChUInt16(..), ChUInt32(..), ChUInt64(..), ChUInt128(..)

  , ChString(..)
  , ChUUID(..)

  , ChArray(..)
  , Nullable
  , LowCardinality, IsLowCardinalitySupported

  , UVarInt(..)
  , module Data.WideWord
  ) where

-- Internal
import Paths_ClickHaskell (version)

-- GHC included
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.DeepSeq (NFData, (<$!!>))
import Control.Exception (Exception, bracketOnError, catch, finally, throwIO, throw, SomeException)
import Control.Monad (forM, replicateM, (<$!>), when)
import Data.Binary.Get
import Data.Binary.Get.Internal (readN)
import Data.Binary.Put
import Data.Bits
import Data.ByteString as BS (StrictByteString, length, take, toStrict)
import Data.ByteString.Builder (Builder, byteString, stringUtf8, toLazyByteString, word8)
import Data.ByteString.Builder as BS (Builder, byteString, word16HexFixed)
import Data.ByteString.Char8 as BS8 (concatMap, length, pack, replicate, singleton)
import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.List (uncons)
import Data.Maybe (listToMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding as Text (encodeUtf8)
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..))
import Data.Vector.Primitive.Mutable (Prim)
import Data.Version (Version (..))
import Data.Word (Word16, Word32, Word8)
import Debug.Trace (traceShowId)
import GHC.Generics
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (AppendSymbol, ErrorMessage (..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal)
import GHC.Word (Word64)
import System.Timeout (timeout)
import Data.Type.Equality (type(==))
import Data.Type.Bool (If)

-- External
import Data.WideWord (Int128 (..), Word128(..))
import Network.Socket as Sock
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

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

withConnection :: Connection -> (ConnectionState -> IO a) -> IO a
withConnection (MkConnection connStateMVar) f =
  withMVar connStateMVar $ \connState ->
    catch
      @ClientError
      (f connState)
      (\err -> throwIO err)

data ConnectionState = MkConnectionState
  { sock     :: Socket
  , user     :: ChString
  , buffer   :: Buffer
  , revision :: ProtocolRevision
  }

data ConnectionError = NoAdressResolved | EstablishTimeout
  deriving (Show, Exception)

openNativeConnection :: HasCallStack => ChCredential -> IO Connection
openNativeConnection MkChCredential{chHost, chPort, chLogin, chPass, chDatabase} = do
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
           setSocketOption sock NoDelay 1
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
      (sendAll sock . toLazyByteString) (serialize revision mkAddendum)
      a <- newMVar MkConnectionState
        { user = toChType chLogin
        , revision
        , sock
        , buffer
        }
      pure (MkConnection a)
    Exception exception -> throwIO (UserError $ DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType otherPacket)


-- * Ping

ping :: HasCallStack => Connection -> IO ()
ping conn = do
  withConnection conn $ \MkConnectionState{sock, revision, buffer} -> do
    (sendAll sock . toLazyByteString)
      (serialize revision mkPingPacket)
    responsePacket <- rawBufferizedRead buffer (deserialize revision)
    case responsePacket of
      Pong                -> pure ()
      Exception exception -> throwIO (UserError $ DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType otherPacket)




-- * Querying

data Table (name :: Symbol) (columns :: [Type])

instance HasColumns (Table name columns) where
  type GetColumns (Table _ columns) = columns

-- ** Selecting

-- *** Simple

selectFrom ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , KnownSymbol name
  , ReadableFrom table record
  )
  =>
  Connection -> IO [record]
selectFrom conn = do
  withConnection conn $ \connState@MkConnectionState{sock, revision, user} -> do
    let query
          = "SELECT " <> readingColumns @table @record
          <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
    (sendAll sock . toLazyByteString)
      (  serialize revision (mkQueryPacket revision user (toChType query))
      <> serialize revision (mkDataPacket "" 0 0)
      )
    handleSelect @table connState pure

select ::
  forall columns record
  .
  ReadableFrom (Columns columns) record
  =>
  Connection -> ChString -> IO [record]
select conn query = do
  withConnection conn $ \connState@MkConnectionState{sock, revision, user} -> do
    (sendAll sock . toLazyByteString)
      (  serialize revision (mkQueryPacket revision user query)
      <> serialize revision (mkDataPacket "" 0 0)
      )
    handleSelect @(Columns columns) connState pure

instance HasColumns (View name columns parameters) where
  type GetColumns (View _ columns _) = columns

data View (name :: Symbol) (columns :: [Type]) (parameters :: [Type])

selectFromView ::
  forall view record name columns parameters passedParameters
  .
  ( ReadableFrom view record
  , KnownSymbol name
  , view ~ View name columns parameters
  , CheckParameters parameters passedParameters
  )
  => Connection -> (Parameters '[] -> Parameters passedParameters) -> IO [record]
selectFromView conn interpreter = do
  withConnection conn $ \connState@MkConnectionState{sock, revision, user} -> do
    let query =
          "SELECT " <> readingColumns @view @record <>
          " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> viewParameters interpreter
    (sendAll sock . toLazyByteString)
      (  serialize revision (mkQueryPacket revision user (toChType query))
      <> serialize revision (mkDataPacket "" 0 0)
      )
    handleSelect @view connState pure

-- *** Streaming

streamSelectFrom ::
  forall table record name columns a
  .
  ( table ~ Table name columns
  , KnownSymbol name
  , ReadableFrom table record
  , NFData a
  )
  =>
  Connection -> ([record] -> IO [a]) -> IO [a]
streamSelectFrom conn f = do
  withConnection conn $ \connState@MkConnectionState{sock, revision, user} -> do
    let query
          = "SELECT " <> readingColumns @table @record
          <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
    (sendAll sock . toLazyByteString)
      (  serialize revision (mkQueryPacket revision user (toChType query))
      <> serialize revision (mkDataPacket "" 0 0)
      )
    let f' x = id <$!!> f x
    handleSelect @table connState f'

streamSelect ::
  forall columns record a
  .
  (ReadableFrom (Columns columns) record, NFData a)
  =>
  Connection -> ChString -> ([record] -> IO [a]) -> IO [a]
streamSelect conn query f = do
  withConnection conn $ \connState@MkConnectionState{sock, revision, user} -> do
    (sendAll sock . toLazyByteString)
      (  serialize revision (mkQueryPacket revision user query)
      <> serialize revision (mkDataPacket "" 0 0)
      )
    let f' x = id <$!!> f x
    handleSelect @(Columns columns) connState f'

streamSelectFromView ::
  forall view record name columns parameters passedParameters a
  .
  ( ReadableFrom view record
  , KnownSymbol name
  , view ~ View name columns parameters
  , NFData a
  , CheckParameters parameters passedParameters
  )
  => Connection -> (Parameters '[] -> Parameters passedParameters) -> ([record] -> IO [a]) -> IO [a]
streamSelectFromView conn interpreter f = do
  withConnection conn $ \connState@MkConnectionState{sock, revision, user} -> do
    let query =
          "SELECT " <> readingColumns @view @record <>
          " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> viewParameters interpreter
    (sendAll sock . toLazyByteString)
      (  serialize revision (mkQueryPacket revision user (toChType query))
      <> serialize revision (mkDataPacket "" 0 0)
      )
    let f' x = id <$!!> f x
    handleSelect @view connState f'

-- *** Internal

handleSelect ::
  forall hasColumns record a
  .
  ReadableFrom hasColumns record
  =>
  ConnectionState -> ([record] -> IO [a])  -> IO [a]
handleSelect conn@MkConnectionState{..} f = do
  packet <- rawBufferizedRead buffer (deserialize revision)
  case packet of
    DataResponse MkDataPacket{columns_count, rows_count} -> do
      case (columns_count, rows_count) of
        (0, 0) -> handleSelect @hasColumns conn f
        (_, rows) -> do
          columns <- rawBufferizedRead buffer (deserializeColumns @hasColumns revision rows)
          processedColumns <- f columns
          (processedColumns ++) <$> handleSelect @hasColumns conn f
    Progress          _ -> handleSelect @hasColumns conn f
    ProfileInfo       _ -> handleSelect @hasColumns conn f
    EndOfStream         -> pure []
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType otherPacket)


-- ** Inserting

insertInto ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , WritableInto table record
  , KnownSymbol name
  , Show record
  )
  => Connection -> [record] -> IO ()
insertInto conn columnsData = do
  withConnection conn $ \connState@MkConnectionState{sock, user, revision} -> do
    let query =
          "INSERT INTO " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
          <> " (" <> writingColumns @table @record <> ") VALUES"
    (sendAll sock . toLazyByteString)
      (  serialize revision (mkQueryPacket revision user (toChType query))
      <> serialize revision (mkDataPacket "" 0 0)
      )
    handleInsertResult @table connState columnsData

handleInsertResult :: forall columns record . (WritableInto columns record, Show record) => ConnectionState -> [record] -> IO ()
handleInsertResult conn@MkConnectionState{..} records = do
  firstPacket <- rawBufferizedRead buffer (deserialize revision)
  case firstPacket of
    TableColumns      _ -> handleInsertResult @columns conn records
    DataResponse MkDataPacket{} -> do
      _emptyDataPacket <- rawBufferizedRead buffer (deserializeInsertHeader @columns @record revision)
      (sendAll sock . toLazyByteString)
        (  serialize revision (mkDataPacket "" (columnsCount @columns @record) (fromIntegral $ Prelude.length records))
        <> serializeRecords @columns revision records
        <> serialize revision (mkDataPacket "" 0 0)
        )
      handleInsertResult @columns @record conn []
    EndOfStream         -> pure ()
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType otherPacket)




-- * Bufferization

data Buffer = MkBuffer
  { bufferSize :: Int
  , bufferSocket :: Socket
  , buff :: IORef StrictByteString
  }

initBuffer :: Int -> Socket -> IO Buffer
initBuffer size sock = MkBuffer size sock <$> newIORef ""

flushBuffer :: Buffer -> IO ()
flushBuffer MkBuffer{buff} = atomicWriteIORef buff ""

rawBufferizedRead :: Buffer -> Get packet -> IO packet
rawBufferizedRead buffer parser = runBufferReader buffer (runGetIncremental parser)

runBufferReader :: Buffer -> Decoder packet -> IO packet
runBufferReader buffer@MkBuffer{bufferSocket, bufferSize, buff} = \case
  (Partial decoder) -> do
    currentBuffer <- readIORef buff
    chosenBuffer <- case BS.length currentBuffer of
      0 -> recv bufferSocket bufferSize
      _ -> flushBuffer buffer $> currentBuffer
    case BS.length chosenBuffer of
      0 -> throwIO (InternalError $ DeserializationError "Expected more bytes while reading packet")
      _ -> runBufferReader buffer (decoder $ Just $! chosenBuffer)
  (Done leftover _consumed packet) -> atomicWriteIORef buff leftover $> packet
  (Fail _leftover _consumed msg) -> throwIO (InternalError $ DeserializationError msg)




-- * Errors handling

data ClientError where
  ConnectionError :: HasCallStack => ConnectionError -> ClientError
  UserError :: HasCallStack => UserError -> ClientError
  InternalError :: HasCallStack => InternalError -> ClientError

instance Show ClientError where
  show (ConnectionError err)   = "ConnectionError " <> show err <> "\n" <> prettyCallStack callStack
  show (UserError err)         = "UserError " <> show err <> "\n" <> prettyCallStack callStack
  show (InternalError err)     = "InternalError " <> show err <> "\n" <> prettyCallStack callStack

deriving anyclass instance Exception ClientError

{- |
  You shouldn't see this exceptions. Please report a bug if it appears
-}
data InternalError
  = UnexpectedPacketType ServerPacketType
  | DeserializationError String
  deriving (Show, Exception)

data UserError
  = UnmatchedType String
  | UnmatchedColumn String
  | DatabaseException ExceptionPacket
  deriving (Show, Exception)




-- * Compatibility

latestSupportedRevision :: ProtocolRevision
latestSupportedRevision = mostRecentRevision

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
  deriving (Enum, Show)

type family PacketTypeNumber (packetType :: ClientPacketType)
  where
  PacketTypeNumber Hello = 0
  PacketTypeNumber Query = 1
  PacketTypeNumber Data = 2
  PacketTypeNumber Cancel = 3
  PacketTypeNumber Ping = 4
  PacketTypeNumber TablesStatusRequest = 5
  PacketTypeNumber ClickHaskell.KeepAlive = 6
  PacketTypeNumber Scalar = 7
  PacketTypeNumber IgnoredPartUUIDs = 8
  PacketTypeNumber ReadTaskResponse = 9
  PacketTypeNumber MergeTreeReadTaskResponse = 10
  PacketTypeNumber SSHChallengeRequest = 11
  PacketTypeNumber SSHChallengeResponse = 12

data Packet (packetType :: ClientPacketType) = MkPacket
instance KnownNat (PacketTypeNumber packetType) => Show (Packet (packetType :: ClientPacketType)) where
  show _ = show . toEnum @ClientPacketType . fromIntegral $ packetNumVal @packetType

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
    , client_name, client_version_major, client_version_minor
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


mkAddendum :: Addendum
mkAddendum = MkAddendum
  { quota_key = MkSinceRevision ""
  }

data Addendum = MkAddendum
  { quota_key :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY
  }
  deriving (Generic, Serializable)


-- ** Ping

mkPingPacket :: PingPacket
mkPingPacket = MkPingPacket{packet_type = MkPacket}

data PingPacket = MkPingPacket{packet_type :: Packet Ping}
  deriving (Generic, Serializable)


-- ** Query

mkQueryPacket :: ProtocolRevision -> ChString -> ChString -> QueryPacket
mkQueryPacket chosenRev user query = MkQueryPacket
  { query_packet = MkPacket
  , query_id = ""
  , client_info                    = MkSinceRevision MkClientInfo
    { query_kind                   = InitialQuery
    , initial_user                 = user
    , initial_query_id             = ""
    , initial_adress               = "0.0.0.0:0"
    , initial_time                 = MkSinceRevision 0
    , interface_type               = 1 -- [tcp - 1, http - 2]
    , os_user                      = "dmitry"
    , hostname                     = "desktop"
    , client_name, client_version_major, client_version_minor
    , client_revision              = chosenRev
    , quota_key                    = MkSinceRevision ""
    , distrubuted_depth            = MkSinceRevision 0
    , client_version_patch
    , open_telemetry               = MkSinceRevision 0
    , collaborate_with_initiator   = MkSinceRevision 0
    , count_participating_replicas = MkSinceRevision 0
    , number_of_current_replica    = MkSinceRevision 0
    }
  , settings           = MkDbSettings
  , interserver_secret = MkSinceRevision ""
  , query_stage        = Complete
  , compression        = 0
  , query              = query
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
  = FetchColumns
  | WithMergeableState
  | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

instance Serializable QueryStage where
  serialize rev = serialize @UVarInt rev . queryStageCode

queryStageCode :: QueryStage -> UVarInt
queryStageCode = \case
  FetchColumns -> 0
  WithMergeableState -> 1
  Complete -> 2
  WithMergeableStateAfterAggregation -> 3
  WithMergeableStateAfterAggregationAndLimit -> 4

data Flags = IMPORTANT | CUSTOM | OBSOLETE
_flagCode :: Flags -> ChUInt64
_flagCode IMPORTANT = 0x01
_flagCode CUSTOM    = 0x02
_flagCode OBSOLETE  = 0x04

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
  , client_version_major         :: UVarInt
  , client_version_minor         :: UVarInt
  , client_revision              :: ProtocolRevision
  , quota_key                    :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
  , distrubuted_depth            :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
  , client_version_patch         :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , open_telemetry               :: ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_OPENTELEMETRY
  , collaborate_with_initiator   :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , count_participating_replicas :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , number_of_current_replica    :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  }
  deriving (Generic, Serializable)

data QueryKind = NoQuery | InitialQuery | SecondaryQuery
  deriving (Enum)

instance Serializable QueryKind where
  serialize rev = serialize @ChUInt8 rev . queryKindToEnum

queryKindToEnum :: QueryKind -> ChUInt8
queryKindToEnum = \case NoQuery -> 1; InitialQuery -> 2; SecondaryQuery -> 3

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
  deriving (Generic, Serializable, Deserializable, Show)

data BlockInfo = MkBlockInfo
  { field_num1   :: UVarInt, is_overflows :: ChUInt8
  , field_num2   :: UVarInt, bucket_num   :: ChInt32
  , eof          :: UVarInt
  }
  deriving (Generic, Serializable, Deserializable, Show)




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

instance Show ServerPacketType where
  show (HelloResponse hello) = "HelloResponse " <> show hello
  show (DataResponse dataPacket) = "DataResponse " <> show dataPacket
  show (Exception exception) = "Exception " <> show exception
  show (Progress progress) = "Progress " <> show progress
  show Pong = "Pong"
  show EndOfStream = "EndOfStream"
  show (ProfileInfo profileInfo) = "ProfileInfo " <> show profileInfo
  show Totals = "Totals"
  show Extremes = "Extremes"
  show TablesStatusResponse = "TablesStatusResponse"
  show Log = "Log"
  show (TableColumns tabelColumnsPacket) = "TableColumns " <> show tabelColumnsPacket
  show UUIDs = "UUIDs"
  show ReadTaskRequest = "ReadTaskRequest"
  show ProfileEvents = "ProfileEvents"
  show (UnknownPacket packetNum) = "UnknownPacket: " <> show packetNum

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
  , read_nonce                     :: ChUInt64 `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2
  }
  deriving (Generic, Show)

instance Deserializable HelloResponse where
  deserialize revision = do
    server_name                    <- deserialize revision
    server_version_major           <- deserialize revision
    server_version_minor           <- deserialize revision
    server_revision                <- deserialize revision
    -- Override current protocol revision for backward compatibility
    let chosenRevision = min server_revision revision
    server_parallel_replicas_proto <- deserialize chosenRevision
    server_timezone                <- deserialize chosenRevision
    server_display_name            <- deserialize chosenRevision
    server_version_patch           <- deserialize chosenRevision
    proto_send_chunked_srv         <- deserialize chosenRevision
    proto_recv_chunked_srv         <- deserialize chosenRevision
    password_complexity_rules      <- deserialize chosenRevision
    read_nonce                     <- deserialize chosenRevision
    pure MkHelloResponse{..}

data PasswordComplexityRules = MkPasswordComplexityRules
  { original_pattern  :: ChString
  , exception_message :: ChString
  }
  deriving (Generic, Deserializable, Show)

instance Deserializable [PasswordComplexityRules] where
  deserialize rev = do
    len <- deserialize @UVarInt rev
    replicateM (fromIntegral len) (deserialize @PasswordComplexityRules rev)

-- ** Exception

data ExceptionPacket = MkExceptionPacket
  { code        :: ChInt32
  , name        :: ChString
  , message     :: ChString
  , stack_trace :: ChString
  , nested      :: ChUInt8
  }
  deriving (Generic, Deserializable, Show)

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
  deriving (Generic, Deserializable, Show)

-- ** ProfileInfo

data ProfileInfo = MkProfileInfo
  { rows                         :: UVarInt
  , blocks                       :: UVarInt
  , bytes                        :: UVarInt
  , applied_limit                :: ChUInt8
  , rows_before_limit            :: UVarInt
  , calculated_rows_before_limit :: ChUInt8
  , applied_aggregation          :: ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  , rows_before_aggregation      :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  }
  deriving (Generic, Deserializable, Show)

-- ** TableColumns

data TableColumns = MkTableColumns
  { table_name :: ChString
  , table_columns :: ChString
  }
  deriving (Generic, Deserializable, Show)








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
  readingColumns = gReadingColumns @(GetColumns hasColumns) @(Rep record)


class GReadable (columns :: [Type]) f
  where
  gFromColumns :: ProtocolRevision -> UVarInt -> Get [f p]
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = map (M1 . M1) <$> gFromColumns @columns rev size
  gReadingColumns = gReadingColumns @columns @f

instance
  GReadable columns (left :*: (right1 :*: right2))
  =>
  GReadable columns ((left :*: right1) :*: right2)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = do
    list <- gFromColumns @columns rev size
    pure [(l :*: r1) :*: r2 | (l :*: (r1 :*: r2)) <- list]
  gReadingColumns = gReadingColumns @columns @(left :*: (right1 :*: right2))


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
    zipWith (:*:)
      <$> gFromColumns @'[Column name chType] rev size
      <*> gFromColumns @restColumns rev size
  gReadingColumns =
    renderColumnName @(Column name chType)
    <> ", " <> gReadingColumns @restColumns @right

instance
  ( KnownColumn (Column name chType)
  , DeserializableColumn (Column name chType)
  , FromChType chType inputType
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  ) => GReadable columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = map (M1 . K1 . fromChType @chType) . columnValues <$> deserializeColumn @(Column name chType) rev size
  gReadingColumns = renderColumnName @(Column name chType)




-- ** Column deserialization

{-# SPECIALIZE replicateM :: Int -> Get chType -> Get [chType] #-}

class DeserializableColumn column where
  deserializeColumn :: ProtocolRevision -> UVarInt -> Get column

handleColumnHeader :: forall column . KnownColumn column => ProtocolRevision -> Get ()
handleColumnHeader rev = do
  let expectedColumnName = toChType (renderColumnName @column)
  resultColumnName <- deserialize @ChString rev
  when (resultColumnName /= expectedColumnName)
    . throw . UserError . UnmatchedColumn
      $ "Got column \"" <> show resultColumnName <> "\" but expected \"" <> show expectedColumnName <> "\""

  let expectedType = toChType (renderColumnType @column)
  resultType <- deserialize @ChString rev
  when (resultType /= expectedType)
    . throw . UserError . UnmatchedType
      $  "Column " <> show resultColumnName <> " has type " <> show resultType <> ". But expected type is " <> show expectedType

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name chType) where
  deserializeColumn rev rows = do
    handleColumnHeader @(Column name chType) rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    column <- replicateM (fromIntegral rows) (deserialize @chType rev)
    pure $ mkColumn @(Column name chType) column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name (Nullable chType)) where
  deserializeColumn rev rows = do
    handleColumnHeader @(Column name (Nullable chType)) rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    nulls <- replicateM (fromIntegral rows) (deserialize @ChUInt8 rev)
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
  , ToChType (LowCardinality chType) chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  DeserializableColumn (Column name (LowCardinality chType)) where
  deserializeColumn rev rows = do
    handleColumnHeader @(Column name (LowCardinality chType)) rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    _serializationType <- (.&. 0xf) <$> deserialize @ChUInt64 rev
    _index_size <- deserialize @ChInt64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    lc <- replicateM (fromIntegral rows) (toChType <$> deserialize @chType rev)
    pure $ mkColumn @(Column name (LowCardinality chType)) lc

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (ChArray chType))
  , Deserializable chType
  , TypeError ('Text "Arrays deserialization still unsupported")
  )
  => DeserializableColumn (Column name (ChArray chType)) where
  deserializeColumn rev _rows = do
    handleColumnHeader @(Column name (ChArray chType)) rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    (arraySize, _offsets) <- traceShowId <$> readOffsets rev
    _types <- replicateM (fromIntegral arraySize) (deserialize @chType rev)
    pure $ mkColumn @(Column name (ChArray chType)) []
    where
    readOffsets :: ProtocolRevision -> Get (ChUInt64, [ChUInt64])
    readOffsets revivion = do
      size <- deserialize @ChUInt64 rev
      (size, ) <$> go size
      where
      go arraySize =
        do
        nextOffset <- deserialize @ChUInt64 revivion
        if arraySize >= nextOffset
          then pure [nextOffset]
          else (nextOffset :) <$> go arraySize


class
  Deserializable chType
  where
  default deserialize :: (Generic chType, GDeserializable (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev


-- ** Generics

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
  GDeserializable (left :*: (right1 :*: right2))
  =>
  GDeserializable ((left :*: right1) :*: right2)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = (\(l :*: (r1 :*: r2)) -> (l :*: r1) :*: r2) <$> gDeserialize rev

instance
  (GDeserializable (S1 metaSel field), GDeserializable right)
  =>
  GDeserializable (S1 metaSel field :*: right)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = (:*:) <$> gDeserialize rev <*> gDeserialize rev

instance
  Deserializable chType
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev =  M1 . K1 <$> deserialize @chType rev


-- ** Database types

instance Deserializable ChUUID where
  deserialize _ = MkChUUID <$!> (flip Word128 <$> getWord64le <*> getWord64le)

instance Deserializable ChString where
  deserialize rev = do
    strSize <- fromIntegral <$> deserialize @UVarInt rev
    toChType <$> readN strSize (BS.take strSize)


instance Deserializable ChInt8 where deserialize _ = toChType <$> getInt8
instance Deserializable ChInt16 where deserialize _ = toChType <$> getInt16le
instance Deserializable ChInt32 where deserialize _ = toChType <$> getInt32le
instance Deserializable ChInt64 where deserialize _ = toChType <$> getInt64le
instance Deserializable ChInt128 where deserialize _ = toChType <$> (flip Int128 <$> getWord64le <*> getWord64le)
instance Deserializable ChUInt8 where deserialize _ = toChType <$> getWord8
instance Deserializable ChUInt16 where deserialize _ = toChType <$> getWord16le
instance Deserializable ChUInt32 where deserialize _ = toChType <$> getWord32le
instance Deserializable ChUInt64 where deserialize _ = toChType <$> getWord64le
instance Deserializable ChUInt128 where deserialize _ = toChType <$> (flip Word128 <$> getWord64le <*> getWord64le)
instance Deserializable (ChDateTime tz) where deserialize _ = toChType <$> getWord32le
instance Deserializable ChDate where deserialize _ = toChType <$> getWord16le

instance Deserializable UVarInt where
  deserialize _ = go 0 (0 :: UVarInt)
    where
    go i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else go (i + 1) $! o'
    go _ _ = fail "input exceeds varuint size"








-- * Columns

-- ** Columns extraction helper

class
  HasColumns hasColumns
  where
  type GetColumns hasColumns :: [Type]

instance HasColumns (Columns columns)
  where
  type GetColumns (Columns columns) = columns


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
  ChUInt8Column :: [ChUInt8] -> Column name ChUInt8
  ChUInt16Column :: [ChUInt16] -> Column name ChUInt16
  ChUInt32Column :: [ChUInt32] -> Column name ChUInt32
  ChUInt64Column :: [ChUInt64] -> Column name ChUInt64
  ChUInt128Column :: [ChUInt128] -> Column name ChUInt128
  ChInt8Column :: [ChInt8] -> Column name ChInt8
  ChInt16Column :: [ChInt16] -> Column name ChInt16
  ChInt32Column :: [ChInt32] -> Column name ChInt32
  ChInt64Column :: [ChInt64] -> Column name ChInt64
  ChInt128Column :: [ChInt128] -> Column name ChInt128
  ChDateColumn :: [ChDate] -> Column name ChDate
  ChDateTimeColumn :: [ChDateTime tz] -> Column name (ChDateTime tz)
  ChUUIDColumn :: [ChUUID] -> Column name ChUUID
  ChStringColumn :: [ChString] -> Column name ChString
  ChArrayColumn :: IsChType chType => [ChArray chType] -> Column name (ChArray chType)
  NullableColumn :: IsChType chType => [Nullable chType] -> Column name (Nullable chType)
  LowCardinalityColumn :: (IsLowCardinalitySupported chType, IsChType chType) => [chType] -> Column name (LowCardinality chType)

type family GetColumnName column :: Symbol
  where
  GetColumnName (Column name columnType) = name

type family GetColumnType column :: Type
  where
  GetColumnType (Column name columnType) = columnType

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

{-# INLINE [0] columnValues #-}
columnValues :: Column name chType -> [chType]
columnValues column = case column of
  (ChUInt8Column values) -> values
  (ChUInt16Column values) -> values
  (ChUInt32Column values) -> values
  (ChUInt64Column values) -> values
  (ChUInt128Column values) -> values
  (ChInt8Column values) -> values
  (ChInt16Column values) -> values
  (ChInt32Column values) -> values
  (ChInt64Column values) -> values
  (ChInt128Column values) -> values
  (ChDateColumn values) -> values
  (ChDateTimeColumn values) -> values
  (ChUUIDColumn values) -> values
  (ChStringColumn values) -> values
  (ChArrayColumn arrayValues) -> arrayValues
  (NullableColumn nullableValues) ->  nullableValues
  (LowCardinalityColumn lowCardinalityValues) -> map fromChType lowCardinalityValues

instance KnownSymbol name => KnownColumn (Column name ChUInt8) where mkColumn = ChUInt8Column
instance KnownSymbol name => KnownColumn (Column name ChUInt16) where mkColumn = ChUInt16Column
instance KnownSymbol name => KnownColumn (Column name ChUInt32) where mkColumn = ChUInt32Column
instance KnownSymbol name => KnownColumn (Column name ChUInt64) where mkColumn = ChUInt64Column
instance KnownSymbol name => KnownColumn (Column name ChUInt128) where mkColumn = ChUInt128Column
instance KnownSymbol name => KnownColumn (Column name ChInt8)  where mkColumn = ChInt8Column
instance KnownSymbol name => KnownColumn (Column name ChInt16) where mkColumn = ChInt16Column
instance KnownSymbol name => KnownColumn (Column name ChInt32) where mkColumn = ChInt32Column
instance KnownSymbol name => KnownColumn (Column name ChInt64) where mkColumn = ChInt64Column
instance KnownSymbol name => KnownColumn (Column name ChInt128) where mkColumn = ChInt128Column
instance KnownSymbol name => KnownColumn (Column name ChDate) where mkColumn = ChDateColumn
instance
  ( KnownSymbol name
  , IsChType (ChDateTime tz)
  ) =>
  KnownColumn (Column name (ChDateTime tz)) where mkColumn = ChDateTimeColumn
instance KnownSymbol name => KnownColumn (Column name ChUUID) where mkColumn = ChUUIDColumn
instance
  ( KnownSymbol name
  , IsChType chType
  , IsChType (Nullable chType)
  ) =>
  KnownColumn (Column name (Nullable chType)) where mkColumn = NullableColumn
instance KnownSymbol name => KnownColumn (Column name ChString) where mkColumn = ChStringColumn
instance
  ( KnownSymbol name
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  ) =>
  KnownColumn (Column name (LowCardinality chType)) where mkColumn = LowCardinalityColumn . map fromChType
instance KnownSymbol name => KnownColumn (Column name (ChArray ChString)) where mkColumn = ChArrayColumn


-- ** Columns

instance
  Serializable (Columns '[])
  where
  {-# INLINE serialize #-}
  serialize _rev Empty = ""

instance
  ( Serializable (Columns columns)
  , Serializable col
  ) =>
  Serializable (Columns (col ': columns))
  where
  {-# INLINE serialize #-}
  serialize rev (AddColumn col columns) = serialize rev col <> serialize rev columns

instance
  ( KnownColumn (Column name chType)
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name chType) where
  {-# INLINE serialize #-}
  serialize rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
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
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    -- Nulls
    <> mconcat (Prelude.map (serialize @ChUInt8 rev . maybe 1 (const 0)) (columnValues column))
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
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> undefined column








-- * Parameters

type family KnownParameter param
  where
  KnownParameter (Parameter name parType) = (KnownSymbol name, ToQueryPart parType)

data Parameter (name :: Symbol) (chType :: Type) = MkParamater chType

data Parameters parameters where
  NoParameters :: Parameters '[]
  AddParameter
    :: KnownParameter (Parameter name chType)
    => Parameter name chType
    -> Parameters parameters
    -> Parameters (Parameter name chType ': parameters)

-- >>> import ClickHaskell.DbTypes
{- |
>>> parameters (parameter @"a3" @ChString ("a3Val" :: String) . parameter @"a2" @ChString ("a2Val" :: String))
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

  default serializeRecords :: GenericWritable record (GetColumns columns) => ProtocolRevision -> [record] -> Builder
  serializeRecords :: ProtocolRevision -> [record] -> Builder
  serializeRecords rev = gSerializeRecords @(GetColumns columns) rev . map from

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
  gDeserializeInsertHeader rev = const () <$> gDeserializeInsertHeader @columns @(left1 :*: (left2 :*: right)) rev
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
  gSerializeRecords rev
    = (\(a, b) -> gSerializeRecords @'[Column name chType] rev a <> gSerializeRecords @restColumns rev b)
    . unzip . map (\(l :*: r) -> (l, r))
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
  gDeserializeInsertHeader rev = const () <$> deserializeColumn @(Column name chType) rev 0
  gWritingColumns = renderColumnName @(Column name chType)
  gColumnsCount = 1

class Serializable chType
  where
  default serialize :: (Generic chType, GSerializable (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from


-- ** Database types
instance Serializable UVarInt where
  serialize _ = go
    where
    go i
      | i < 0x80 = word8 (fromIntegral i)
      | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)

instance Serializable ChString where
  serialize rev str
    =  (serialize @UVarInt rev . fromIntegral . BS.length . fromChType) str
    <> (execPut . putByteString . fromChType) str

instance Serializable ChUUID where serialize _ = execPut . (\(hi, lo) -> putWord64le lo <> putWord64le hi) . fromChType
instance Serializable ChInt8 where serialize _ = execPut . putInt8 . fromChType
instance Serializable ChInt16 where serialize _ = execPut . putInt16le . fromChType
instance Serializable ChInt32 where serialize _ = execPut . putInt32le . fromChType
instance Serializable ChInt64 where serialize _ = execPut . putInt64le . fromChType
instance Serializable ChInt128 where serialize _ = execPut . (\(Int128 hi lo) -> putWord64le lo <> putWord64le hi) . fromChType
instance Serializable ChUInt8 where serialize _ = execPut . putWord8 . fromChType
instance Serializable ChUInt16 where serialize _ = execPut . putWord16le . fromChType
instance Serializable ChUInt32 where serialize _ = execPut . putWord32le . fromChType
instance Serializable ChUInt64 where serialize _ = execPut . putWord64le . fromChType
instance Serializable ChUInt128 where serialize _ = execPut . (\(Word128 hi lo) -> putWord64le lo <> putWord64le hi) . fromChType
instance Serializable (ChDateTime tz) where serialize _ = execPut . putWord32le . fromChType
instance Serializable ChDate where serialize _ = execPut . putWord16le . fromChType


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








class
  KnownSymbol (ToChTypeName chType) =>
  IsChType chType
  where
  -- | Shows database original type name
  --
  -- @
  -- type ToChTypeName ChString = \"String\"
  -- type ToChTypeName (Nullable ChUInt32) = \"Nullable(UInt32)\"
  -- @
  type ToChTypeName chType :: Symbol

  chTypeName :: Builder
  chTypeName = byteString . BS8.pack . symbolVal @(ToChTypeName chType) $ Proxy

  defaultValueOfTypeName :: chType

class ToChType chType inputType    where toChType    :: inputType -> chType
class FromChType chType outputType where fromChType  :: chType -> outputType
class ToQueryPart chType           where toQueryPart :: chType -> BS.Builder

instance {-# OVERLAPPABLE #-} (IsChType chType, chType ~ inputType) => ToChType chType inputType where toChType = id
instance {-# OVERLAPPABLE #-} (IsChType chType, chType ~ inputType) => FromChType chType inputType where fromChType = id



-- | ClickHouse Nullable(T) column type
-- (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

{-
This instance leads to disable -Wmissing-methods
Need to move it's semantics to another instances

instance {-# OVERLAPPING #-}
  ( TypeError
    (     'Text (ToChTypeName (Nullable (LowCardinality chType))) ':<>: 'Text " is unsupported type in ClickHouse."
    ':$$: 'Text "Use " ':<>: 'Text (ToChTypeName (LowCardinality (Nullable chType))) ':<>: 'Text " instead."
    )
  , IsChType chType
  ) => IsChType (Nullable (LowCardinality chType))
  where
  defaultValueOfTypeName = error "Unreachable"
  chTypeName = error "Unreachable"
-}

instance
  ( IsChType chType
  , KnownSymbol ("Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")")
  )
  =>
  IsChType (Nullable chType)
  where
  type ToChTypeName (Nullable chType) = NullableTypeName chType
  defaultValueOfTypeName = Nothing

instance
  ( ToQueryPart chType
  , IsChType (Nullable chType)
  )
  =>
  ToQueryPart (Nullable chType)
  where
  toQueryPart = maybe "null" toQueryPart

instance
  ( ToChType inputType chType
  , IsChType (Nullable inputType)
  )
  =>
  ToChType (Nullable inputType) (Nullable chType)
  where
  toChType = fmap (toChType @inputType @chType)

instance
  ( FromChType chType inputType
  , IsChType (Nullable chType)
  )
  =>
  FromChType (Nullable chType) (Nullable inputType)
  where
  fromChType = fmap (fromChType @chType)




-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
deriving newtype instance (Eq chType, IsLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving newtype instance (Show chType, IsLowCardinalitySupported chType) => Show (LowCardinality chType)
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
    ':$$: 'Text "  ChDateTime"
    ':$$: 'Text "  Nullable(T)"
    )
  ) => IsLowCardinalitySupported chType

instance
  ( IsLowCardinalitySupported chType
  , KnownSymbol ("LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")")
  ) =>
  IsChType (LowCardinality chType)
  where
  type ToChTypeName (LowCardinality chType) = "LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  defaultValueOfTypeName = MkLowCardinality $ defaultValueOfTypeName @chType

instance
  ( ToChType inputType chType
  , IsChType (LowCardinality inputType)
  , IsLowCardinalitySupported inputType
  )
  =>
  ToChType (LowCardinality inputType) chType
  where
  toChType = MkLowCardinality . toChType

instance {-# OVERLAPPING #-}
  ( IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  )
  =>
  ToChType (LowCardinality chType) chType
  where
  toChType = MkLowCardinality

instance IsLowCardinalitySupported chType => ToChType chType (LowCardinality chType)
  where
  toChType (MkLowCardinality value) = value

instance IsLowCardinalitySupported chType => FromChType chType (LowCardinality chType)
  where
  fromChType = MkLowCardinality

instance
  ( FromChType chType outputType
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  )
  =>
  FromChType (LowCardinality chType) outputType
  where
  fromChType (MkLowCardinality value) = fromChType value

instance {-# OVERLAPPING #-}
  ( IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  )
  =>
  FromChType (LowCardinality chType) chType
  where
  fromChType (MkLowCardinality value) = value

instance
  ( ToQueryPart chType
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  )
  =>
  ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType




-- | ClickHouse UUID column type
newtype ChUUID = MkChUUID Word128
  deriving newtype (Generic, Show, Eq, NFData, Bounded, Prim, Enum)

instance IsChType ChUUID where
  type ToChTypeName ChUUID = "UUID"
  defaultValueOfTypeName = MkChUUID 0

instance ToQueryPart ChUUID where
  toQueryPart (MkChUUID (Word128 hi lo)) = mconcat
    ["'", p 3 hi, p 2 hi, "-", p 1 hi, "-", p 0 hi, "-", p 3 lo, "-", p 2 lo, p 1 lo, p 0 lo, "'"]
    where
    p :: Int -> Word64 -> Builder
    p shiftN word = word16HexFixed $ fromIntegral (word `unsafeShiftR` (shiftN*16))


instance ToChType ChUUID Word64 where toChType = MkChUUID . flip Word128 0
instance ToChType ChUUID (Word64, Word64) where toChType = MkChUUID . uncurry (flip Word128)

instance FromChType ChUUID (Word64, Word64) where fromChType (MkChUUID (Word128 w64hi w64lo)) = (w64hi, w64lo)




-- | ClickHouse String column type
newtype ChString = MkChString StrictByteString
  deriving newtype (Show, Eq, IsString, NFData)

instance IsChType ChString where
  type ToChTypeName ChString = "String"
  defaultValueOfTypeName = ""


instance ToQueryPart ChString where toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"

escapeQuery :: StrictByteString -> Builder
escapeQuery -- [ClickHaskell.DbTypes.ToDo.1]: Optimize
  = BS.byteString
  . BS8.concatMap (\case '\'' -> "\\\'"; '\\' -> "\\\\"; sym -> BS8.singleton sym;)

instance ToChType ChString StrictByteString where toChType = MkChString
instance ToChType ChString Builder          where toChType = MkChString . toStrict . toLazyByteString
instance ToChType ChString String           where toChType = MkChString . BS8.pack
instance ToChType ChString Text             where toChType = MkChString . Text.encodeUtf8
instance ToChType ChString Int              where toChType = MkChString . BS8.pack . show

instance FromChType ChString StrictByteString where fromChType (MkChString string) = string
instance
  ( TypeError
    (     'Text "ChString to Text using FromChType convertion could cause exception"
    ':$$: 'Text "Decode ByteString manually if you are sure it's always can be decoded or replace it with ByteString"
    )
  ) =>
  FromChType ChString Text
  where
  fromChType = error "Unreachable"




-- | ClickHouse Int8 column type
newtype ChInt8 = MkChInt8 Int8
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt8 where
  type ToChTypeName ChInt8 = "Int8"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt8
  where
  toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt8 Int8   where toChType = MkChInt8

instance FromChType ChInt8 Int8   where fromChType = coerce




-- | ClickHouse Int16 column type
newtype ChInt16 = MkChInt16 Int16
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt16 where
  type ToChTypeName ChInt16 = "Int16"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt16 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt16 Int16   where toChType = MkChInt16

instance FromChType ChInt16 Int16   where fromChType (MkChInt16 int16) = int16




-- | ClickHouse Int32 column type
newtype ChInt32 = MkChInt32 Int32
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt32 where
  type ToChTypeName ChInt32 = "Int32"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt32 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt32 Int32   where toChType = MkChInt32

instance FromChType ChInt32 Int32   where fromChType (MkChInt32 int32) = int32




-- | ClickHouse Int64 column type
newtype ChInt64 = MkChInt64 Int64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt64 where
  type ToChTypeName ChInt64 = "Int64"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt64 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt64 Int64   where toChType = MkChInt64 . fromIntegral
instance ToChType ChInt64 Int     where toChType = MkChInt64 . fromIntegral

instance FromChType ChInt64 Int64   where fromChType = coerce




-- | ClickHouse Int128 column type
newtype ChInt128 = MkChInt128 Int128
  deriving newtype (Show, Eq, Num, Prim, Bits, Ord, Real, Enum, Integral, Bounded, NFData)

instance IsChType ChInt128 where
  type ToChTypeName ChInt128 = "Int128"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt128 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt128 Int128   where toChType = MkChInt128 . fromIntegral

instance FromChType ChInt128 Int128   where fromChType (MkChInt128 int128) = int128




-- | ClickHouse UInt8 column type
newtype ChUInt8 = MkChUInt8 Word8
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt8 where
  type ToChTypeName ChUInt8 = "UInt8"
  defaultValueOfTypeName = 0


instance ToQueryPart ChUInt8 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt8 Word8   where toChType = MkChUInt8

instance FromChType ChUInt8 Word8   where fromChType (MkChUInt8 w8) = w8




-- | ClickHouse UInt16 column type
newtype ChUInt16 = MkChUInt16 Word16
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt16 where
  type ToChTypeName ChUInt16 = "UInt16"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt16 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt16 Word16   where toChType = coerce

instance FromChType ChUInt16 Word16   where fromChType = coerce




-- | ClickHouse UInt32 column type
newtype ChUInt32 = MkChUInt32 Word32
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt32 where
  type ToChTypeName ChUInt32 = "UInt32"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt32 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt32 Word32   where toChType = MkChUInt32

instance FromChType ChUInt32 Word32   where fromChType (MkChUInt32 word32) = word32




-- | ClickHouse UInt64 column type
newtype ChUInt64 = MkChUInt64 Word64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt64 where
  type ToChTypeName ChUInt64 = "UInt64"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt64 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt64 Word64   where toChType = MkChUInt64

instance FromChType ChUInt64 Word64   where fromChType (MkChUInt64 w64) = w64




-- | ClickHouse UInt128 column type
newtype ChUInt128 = MkChUInt128 Word128
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt128 where
  type ToChTypeName ChUInt128 = "UInt128"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt128 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt128 Word128   where toChType = MkChUInt128
instance ToChType ChUInt128 Word64    where toChType = MkChUInt128 . fromIntegral

instance FromChType ChUInt128 Word128   where fromChType (MkChUInt128 w128) = w128




{- |
ClickHouse DateTime column type (paramtrized with timezone)

>>> chTypeName @(ChDateTime "")
"DateTime"
>>> chTypeName @(ChDateTime "UTC")
"DateTime('UTC')"
-}
newtype ChDateTime (tz :: Symbol) = MkChDateTime Word32
  deriving newtype (Show, Eq, Prim, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance KnownSymbol (ToChTypeName (ChDateTime tz)) => IsChType (ChDateTime tz)
  where
  type ToChTypeName (ChDateTime tz) = If (tz == "") ("DateTime") ("DateTime('" `AppendSymbol` tz `AppendSymbol` "')")
  defaultValueOfTypeName = MkChDateTime 0

instance (IsChType (ChDateTime tz)) => ToQueryPart (ChDateTime tz)
  where
  toQueryPart chDateTime = let time = BS8.pack . show . fromChType @(ChDateTime tz) @Word32 $ chDateTime
    in BS.byteString (BS8.replicate (10 - BS8.length time) '0' <> time)

instance ToChType (ChDateTime tz) Word32     where toChType = MkChDateTime
instance ToChType (ChDateTime tz) UTCTime    where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType (ChDateTime tz) ZonedTime  where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC

instance FromChType (ChDateTime tz) Word32     where fromChType = coerce
instance FromChType (ChDateTime tz) UTCTime    where fromChType (MkChDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)




newtype ChDate = MkChDate Word16
  deriving newtype (Show, Eq, Prim, Bits, Bounded, Enum, NFData)

instance IsChType ChDate where
  type ToChTypeName ChDate = "Date"
  defaultValueOfTypeName = MkChDate 0

instance ToChType ChDate Word16 where toChType = MkChDate

instance FromChType ChDate Word16 where fromChType = coerce




newtype ChArray a = MkChArray [a]
  deriving newtype (Show, Eq, NFData)

instance
  ( IsChType chType
  , KnownSymbol (AppendSymbol (AppendSymbol "Array(" (ToChTypeName chType)) ")")
  ) =>
  IsChType (ChArray chType)
  where
  type ToChTypeName (ChArray chType) = "Array(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  defaultValueOfTypeName = MkChArray []




instance
  ( ToQueryPart chType
  , IsChType (ChArray chType)
  ) =>
  ToQueryPart (ChArray chType)
  where
  toQueryPart
    = (\x -> "[" <> x <> "]")
    . (maybe "" (uncurry (foldr (\ a b -> a <> "," <> b)))
    . uncons
    . map (toQueryPart @chType))
    . fromChType

instance
  FromChType (ChArray chType) [chType] where fromChType (MkChArray values) = values

instance
  ( ToChType chType inputType
  ) =>
  ToChType (ChArray chType) [inputType] where toChType = MkChArray . map toChType




{- |
  Unsigned variable-length quantity encoding
  
  Part of protocol implementation
-}
newtype UVarInt = MkUVarInt Word64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)








-- * Versioning

client_version_major, client_version_minor :: UVarInt
client_version_patch :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
client_version_major = case versionBranch version of (x:_) -> fromIntegral x; _ -> 0
client_version_minor = case versionBranch version of (_:x:_) -> fromIntegral x; _ -> 0
client_version_patch = MkSinceRevision $ case versionBranch version of (_:_:x:_) -> fromIntegral x; _ -> 0

client_name :: ChString
client_name = fromString $
  "ClickHaskell-"
  <> show client_version_major <> "."
  <> show client_version_minor <> "."
  <> show client_version_patch

newtype ProtocolRevision = MkProtocolRevision Word64
  deriving newtype (Show, Eq, Num, Ord)

instance Deserializable ProtocolRevision where deserialize = coerce <$> deserialize @UVarInt
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

{-# INLINE [0] mostRecentRevision #-}
mostRecentRevision :: ProtocolRevision
mostRecentRevision = (fromIntegral . natVal) (Proxy @DBMS_TCP_PROTOCOL_VERSION)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented
instance Show a => Show (SinceRevision a revisionNumber) where
  show (MkSinceRevision a) = show a
  show NotPresented = ""

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

instance
  ( KnownNat revision
  , Serializable chType
  )
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
--type DBMS_MIN_REVISION_WITH_TABLES_STATUS = 54226;
--type DBMS_MIN_REVISION_WITH_TIME_ZONE_PARAMETER_IN_DATETIME_DATA_TYPE = 54337;
type DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME = 54372;
type DBMS_MIN_REVISION_WITH_VERSION_PATCH = 54401;
--type DBMS_MIN_REVISION_WITH_SERVER_LOGS = 54406;
--type DBMS_MIN_REVISION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 54448;
--type DBMS_MIN_MAJOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 21;
--type DBMS_MIN_MINOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 4;
--type DBMS_MIN_REVISION_WITH_COLUMN_DEFAULTS_METADATA = 54410;
--type DBMS_MIN_REVISION_WITH_LOW_CARDINALITY_TYPE = 54405;
type DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO = 54420;
--type DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS = 54429;
--type DBMS_MIN_REVISION_WITH_SCALARS = 54429;
type DBMS_MIN_REVISION_WITH_OPENTELEMETRY = 54442;
--type DBMS_MIN_REVISION_WITH_AGGREGATE_FUNCTIONS_VERSIONING = 54452;
--type DBMS_CLUSTER_PROCESSING_PROTOCOL_VERSION = 1;
--type DBMS_MIN_SUPPORTED_PARALLEL_REPLICAS_PROTOCOL_VERSION = 3;
--type DBMS_PARALLEL_REPLICAS_MIN_VERSION_WITH_MARK_SEGMENT_SIZE_FIELD = 4;
--type DBMS_PARALLEL_REPLICAS_PROTOCOL_VERSION = 4;
type DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS = 54453;
--type DBMS_MERGE_TREE_PART_INFO_VERSION = 1;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET = 54441;
--type DBMS_MIN_REVISION_WITH_X_FORWARDED_FOR_IN_CLIENT_INFO = 54443;
--type DBMS_MIN_REVISION_WITH_REFERER_IN_CLIENT_INFO = 54447;
type DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH = 54448;
--type DBMS_MIN_PROTOCOL_VERSION_WITH_INCREMENTAL_PROFILE_EVENTS = 54451;
type DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION = 54454;
type DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME = 54449;
--type DBMS_MIN_PROTOCOL_VERSION_WITH_PROFILE_EVENTS_IN_INSERT = 54456;
--type DBMS_MIN_PROTOCOL_VERSION_WITH_VIEW_IF_PERMITTED = 54457;
--type DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS = 54459;
--type DBMS_MIN_PROTOCOL_VERSION_WITH_SERVER_QUERY_TIME_IN_PROGRESS = 54460;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES = 54461;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2 = 54462;
type DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS = 54463;
--type DBMS_MIN_PROTOCOL_VERSION_WITH_TIMEZONE_UPDATES = 54464;
--type DBMS_MIN_REVISION_WITH_SPARSE_SERIALIZATION = 54465;
--type DBMS_MIN_REVISION_WITH_SSH_AUTHENTICATION = 54466;
--type DBMS_MIN_REVISION_WITH_TABLE_READ_ONLY_CHECK = 54467;
--type DBMS_MIN_REVISION_WITH_SYSTEM_KEYWORDS_TABLE = 54468;
type DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION = 54469;
type DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS = 54470;
type DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL = 54471;
