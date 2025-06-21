{-# OPTIONS_GHC
  -Wno-orphans
  -Wno-unused-imports
  -Wno-unticked-promoted-constructors
#-}

{- |
  Module:      ClickHaskell
  Copyright:   (c) 2023 Dmitry Kovalev
  License:     BSD-3-Clause
  Maintainer:  Dmitry Kovalev
  Stability:   Experimental

  For full documentation, visit: https://clickhaskell.dev/
-}

module ClickHaskell
  (
  {- * Connection -}
    ConnectionArgs, defaultConnectionArgs
  , setHost, setPort, setUser, setDatabase, setPassword
  , overrideNetwork
  , Connection(..), openConnection
  , Buffer(..)

  {- * Errors -}
  , ClientError(..)
  , ConnectionError(..)
  , UserError(..)
  , InternalError(..)

  {- * Client wrappers -}
  {- ** SELECT -}
  , select, selectFrom, selectFromView, generateRandom
  , ClickHaskell(..), FromChType(fromChType)
  {- ** INSERT -}
  , insertInto
  , ToChType(toChType)
  {- ** Arbitrary commands -}, command, ping
  {- ** Shared -}
  , Column, KnownColumn, SerializableColumn
  , Table, View
  {- *** Query -}
  , ToQueryPart(toQueryPart), parameter, Parameter, Parameters, viewParameters

  {- * ClickHouse types -}
  , IsChType(chTypeName, defaultValueOfTypeName)
  , DateTime(..), DateTime64
  , Int8, Int16, Int32, Int64, Int128(..)
  , UInt8, UInt16, UInt32, UInt64, UInt128, Word128(..)
  , Nullable
  , LowCardinality, IsLowCardinalitySupported
  , UUID(..)
  , Array(..)
  , ChString(..)


  {- * Protocol parts -}

  {- ** Shared -}
  , UVarInt(..), SinceRevision(..), ProtocolRevision
  {- *** Data packet -}, DataPacket(..), BlockInfo(..)

  {- ** Client -}, ClientPacket(..)
  {- *** Hello -}, HelloPacket(..), Addendum(..)
  {- *** Query -}
  , QueryPacket(..)
  , DbSettings(..), QueryParameters(..), QueryStage(..)
  , ClientInfo(..), QueryKind(..)
  
  {- ** Server -}, ServerPacket(..)
  {- *** Hello -}, HelloResponse(..), PasswordComplexityRules(..)
  {- *** Exception -}, ExceptionPacket(..)
  {- *** Progress -}, ProgressPacket(..)
  {- *** ProfileInfo -}, ProfileInfo(..)
  {- *** TableColumns -}, TableColumns(..)
  ) where

-- Internal
import Paths_ClickHaskell (version)

-- GHC included
import Control.Applicative (liftA2)
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData)
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, mask, onException, throw, throwIO)
import Control.Monad (forM, replicateM, when, (<$!>), (<=<))
import Data.Binary.Get
import Data.Bits (Bits (setBit, unsafeShiftL, unsafeShiftR, (.&.), (.|.)))
import Data.ByteString as BS (ByteString, length)
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS8 (concatMap, length, pack, replicate, singleton)
import Data.ByteString.Lazy as BSL (toStrict, ByteString)
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.List (uncons)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding as Text (encodeUtf8)
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..))
import Data.Version (Version (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (C1, D1, Generic (..), K1 (K1, unK1), M1 (M1, unM1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (ErrorMessage (..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

-- External
import Data.WideWord (Int128 (..), Word128(..))
import Network.Socket hiding (SocketOption(..))
import Network.Socket qualified as Sock (SocketOption(..))
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

-- * Connection

{- |
  See `defaultConnectionArgs` for documentation
-}
data ConnectionArgs = MkConnectionArgs
  { user :: Text
  , pass :: Text
  , db   :: Text
  , host :: HostName
  , mPort :: Maybe ServiceName
  , isTLS :: Bool
  , initBuffer :: HostName -> SockAddr -> Socket -> IO Buffer
  }

{- |
  Default connection settings which follows __clickhouse-client__ defaults

  Use `setUser`, `setPassword`, `setHost`, `setPort`, `setDatabase`
  to modify connection defaults.

  Or 'setSecure', 'overrideTLS' to configure TLS connection
-}
defaultConnectionArgs :: ConnectionArgs
defaultConnectionArgs = MkConnectionArgs
  { user = "default"
  , pass = ""
  , host = "localhost"
  , db   = "default"
  , isTLS = False
  , mPort = Nothing
  , initBuffer  = \_hostname addrAddress sock -> do
      setSocketOption sock Sock.NoDelay 1
      setSocketOption sock Sock.KeepAlive 1
      connect sock addrAddress
      buff <- newIORef ""
      pure
        MkBuffer
          { writeSock = \bs -> sendAll sock bs
          , readSock  = recv sock 4096
          , closeSock = close sock
          , buff
          }
  }


{- |
  Overrides default user __"default"__
-}
setUser :: Text -> ConnectionArgs -> ConnectionArgs
setUser new MkConnectionArgs{..} = MkConnectionArgs{user=new, ..}

{- |
  Overrides default password __""__
-}
setPassword :: Text -> ConnectionArgs -> ConnectionArgs
setPassword new MkConnectionArgs{..} = MkConnectionArgs{pass=new, ..}

{- |
  Overrides default hostname __"localhost"__
-}
setHost :: HostName -> ConnectionArgs -> ConnectionArgs
setHost new MkConnectionArgs{..} = MkConnectionArgs{host=new, ..}

{- |
  Overrides default port __9000__ (or __9443__ for TLS)
-}
setPort :: ServiceName -> ConnectionArgs -> ConnectionArgs
setPort new MkConnectionArgs{..} = MkConnectionArgs{mPort=Just new, ..} 

{- |
  Overrides default database __"default"__
-}
setDatabase :: Text -> ConnectionArgs -> ConnectionArgs
setDatabase new MkConnectionArgs{..} = MkConnectionArgs{db=new, ..}

overrideNetwork
  :: Bool
  -> (HostName -> SockAddr -> Socket -> IO Buffer)
  -> (ConnectionArgs -> ConnectionArgs)
overrideNetwork new new2 = \MkConnectionArgs{..} ->
  MkConnectionArgs{isTLS=new, initBuffer=new2, ..}

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
  { user     :: ChString
  , hostname :: ChString
  , os_user  :: ChString
  , buffer   :: Buffer
  , revision :: ProtocolRevision
  , creds    :: ConnectionArgs
  }

writeToConnection :: Serializable packet => ConnectionState -> packet -> IO ()
writeToConnection MkConnectionState{revision, buffer} packet =
  (writeSock buffer . toLazyByteString . serialize revision) packet

writeToConnectionEncode :: ConnectionState -> (ProtocolRevision -> Builder) -> IO ()
writeToConnectionEncode MkConnectionState{revision, buffer} serializer =
  (writeSock buffer . toLazyByteString) (serializer revision)

openConnection :: HasCallStack => ConnectionArgs -> IO Connection
openConnection creds = fmap MkConnection . newMVar =<< createConnectionState creds

reopenConnection :: ConnectionState -> IO ConnectionState
reopenConnection MkConnectionState{creds, buffer} = do
  flushBuffer buffer
  closeSock buffer
  createConnectionState creds

createConnectionState :: ConnectionArgs -> IO ConnectionState
createConnectionState creds@MkConnectionArgs {user, pass, db, host, mPort, initBuffer, isTLS} = do
  let port = fromMaybe (if isTLS then "9440" else "9000") mPort
  hostname <- maybe "" toChType <$> lookupEnv "HOSTNAME"
  os_user <- maybe "" toChType <$> lookupEnv "USER"
  AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress}
    <- maybe (throwIO NoAdressResolved) pure . listToMaybe
    =<< getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just host)
      (Just port)
  buffer <- maybe (throwIO EstablishTimeout) pure
    =<< timeout 3_000_000 (
      bracketOnError
        (socket addrFamily addrSocketType addrProtocol)
        (\sock ->
          catch @SomeException
            (finally (shutdown sock ShutdownBoth) (close sock))
            (const $ pure ())
        )
        (\sock -> initBuffer host addrAddress sock)
      )

  (writeSock buffer . toLazyByteString . serialize latestSupportedRevision)
    (mkHelloPacket db user pass)
  serverPacketType <- rawBufferizedRead buffer (deserialize latestSupportedRevision)
  case serverPacketType of
    HelloResponse MkHelloResponse{server_revision} -> do
      let revision = min server_revision latestSupportedRevision
          conn = MkConnectionState{user = toChType user, ..}
      writeToConnection conn MkAddendum{quota_key = MkSinceRevision ""}
      pure conn
    Exception exception -> throwIO (UserError $ DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


{- |
  Might be used for any command without data responses

  For example: CREATE, TRUNCATE, KILL, SET, GRANT

  __Throws exception if any data was returned__
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
    writeToConnection connState Ping
    responsePacket <- rawBufferizedRead buffer (deserialize revision)
    case responsePacket of
      Pong                -> pure ()
      Exception exception -> throwIO (UserError $ DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)




-- * Client wrappers

-- ** SELECT

select ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  Connection -> ChString -> ([output] -> IO result) -> IO [result]
select conn query f = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState query)
    writeToConnection connState (mkDataPacket "" 0 0)
    handleSelect @columns connState (\x -> id <$!> f x)

selectFrom ::
  forall table output result
  .
  ClickHaskellTable table output
  =>
  Connection -> ([output] -> IO result) -> IO [result]
selectFrom conn f = select @(GetColumns table) conn query f
  where
  query = toChType $
    "SELECT " <> columns @(GetColumns table) @output <>
    " FROM " <> tableName @table

selectFromView ::
  forall view output result parameters
  .
  ClickHaskellView view output
  =>
  Connection -> (Parameters '[] -> Parameters parameters) -> ([output] -> IO result) -> IO [result]
selectFromView conn interpreter f = select @(GetColumns view) conn query f
  where
  query = toChType $
    "SELECT " <> columns @(GetColumns view) @output <>
    " FROM " <> tableName @view <> viewParameters interpreter

generateRandom ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  Connection -> (UInt64, UInt64, UInt64) -> UInt64 -> ([output] -> IO result) -> IO [result]
generateRandom conn (randomSeed, maxStrLen, maxArrayLen) limit f = select @columns conn query f
  where
  query = toChType $
    "SELECT * FROM generateRandom(" <>
        "'" <> readingColumnsAndTypes @columns @output <> "' ," <>
          toQueryPart randomSeed <> "," <>
          toQueryPart maxStrLen <> "," <>
          toQueryPart maxArrayLen <>
      ")" <>
    " LIMIT " <> toQueryPart limit <> ";"

-- | Internal
handleSelect ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  ConnectionState -> ([output] -> IO result) -> IO [result]
handleSelect MkConnectionState{..} f = loop []
  where
  loop acc = rawBufferizedRead buffer (deserialize revision) >>=
    \packet -> case packet of
      DataResponse MkDataPacket{columns_count = 0, rows_count = 0} -> loop acc
      DataResponse MkDataPacket{columns_count, rows_count} -> do
        let expected = columnsCount @columns @output
        when (columns_count /= expected) $
          (throw . UserError . UnmatchedColumnsCount)
            ("Expected " <> show expected <> " columns but got " <> show columns_count)
        result <- f =<< rawBufferizedRead buffer (deserializeColumns @columns True revision rows_count)
        loop (result : acc)
      Progress    _       -> loop acc
      ProfileInfo _       -> loop acc
      EndOfStream         -> pure acc
      Exception exception -> throwIO (UserError $ DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


-- ** INSERT

insertInto ::
  forall table record
  .
  ClickHaskellTable table record
  =>
  Connection -> [record] -> IO ()
insertInto conn columnsData = do
  withConnection conn $ \connState -> do
    writeToConnection connState (mkQueryPacket connState query)
    writeToConnection connState (mkDataPacket "" 0 0)
    handleInsertResult @(GetColumns table) connState columnsData
  where
  query = toChType $
    "INSERT INTO " <> tableName @table
    <> " (" <> columns @(GetColumns table) @record <> ") VALUES"

-- | Internal
handleInsertResult :: forall columns record . ClickHaskell columns record => ConnectionState -> [record] -> IO ()
handleInsertResult conn@MkConnectionState{..} records = do
  firstPacket <- rawBufferizedRead buffer (deserialize revision)
  case firstPacket of
    TableColumns      _ -> handleInsertResult @columns conn records
    DataResponse MkDataPacket{} -> do
      _emptyDataPacket <- rawBufferizedRead buffer (deserializeColumns @columns @record False revision 0)
      writeToConnection conn (mkDataPacket "" (columnsCount @columns @record) (fromIntegral $ Prelude.length records))
      writeToConnectionEncode conn (serializeRecords @columns records)
      writeToConnection conn (mkDataPacket "" 0 0)
      handleInsertResult @columns @record conn []
    EndOfStream         -> pure ()
    Exception exception -> throwIO (UserError $ DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)

-- ** Common parts

type family GetTableName table :: Symbol
type instance (GetTableName (Table name columns)) = name
type instance (GetTableName (View name columns params)) = name

type family GetColumns table :: [Type]
type instance (GetColumns (Table name columns)) = columns
type instance GetColumns (View name columns params) = columns

tableName :: forall table . KnownSymbol (GetTableName table) => Builder
tableName = (byteString . BS8.pack) (symbolVal $ Proxy @(GetTableName table))

class IsTable table

-- | Type wrapper for statements generation
data Table (name :: Symbol) (columns :: [Type])
instance IsTable (Table name columns) where

type ClickHaskellTable table record =
  ( IsTable table
  , KnownSymbol (GetTableName table)
  , ClickHaskell (GetColumns table) record
  )


class IsView view

-- | Type wrapper for statements generation
data View (name :: Symbol) (columns :: [Type]) (parameters :: [Type])
instance IsView (View name columns parameters)

type ClickHaskellView view record =
  ( IsView view
  , KnownSymbol (GetTableName view)
  , ClickHaskell (GetColumns view) record
  )




-- * Bufferization

data Buffer = MkBuffer
  { readSock :: IO BS.ByteString
  , writeSock :: BSL.ByteString -> IO ()
  , closeSock :: IO ()
  , buff :: IORef BS.ByteString
  }

flushBuffer :: Buffer -> IO ()
flushBuffer MkBuffer{buff} = atomicWriteIORef buff ""

rawBufferizedRead :: Buffer -> Get packet -> IO packet
rawBufferizedRead buffer@MkBuffer{..} parser = runBufferReader (runGetIncremental parser)
  where
  runBufferReader :: Decoder packet -> IO packet
  runBufferReader = \case
    (Partial decoder) -> readBuffer >>= runBufferReader . decoder . Just
    (Done leftover _consumed packet) -> packet <$ atomicModifyIORef buff (leftover,)
    (Fail _leftover _consumed msg) -> throwIO (InternalError $ DeserializationError msg)

  readBuffer :: IO BS.ByteString
  readBuffer =
    readIORef buff
      >>= (\currentBuffer ->
        case BS.length currentBuffer of
          0 -> readSock
          _ -> flushBuffer buffer *> pure currentBuffer
      )




-- * Errors handling

{- |
  A wrapper for all client-related errors
-}
data ClientError where
  UserError :: HasCallStack => UserError -> ClientError
  InternalError :: HasCallStack => InternalError -> ClientError
  deriving anyclass (Exception)

instance Show ClientError where
  show (UserError err)     = "UserError " <> show err <> "\n" <> prettyCallStack callStack
  show (InternalError err) = "InternalError " <> show err <> "\n" <> prettyCallStack callStack

{- |
  Errors occured on connection operations
-}
data ConnectionError
  = NoAdressResolved
  -- ^ Occurs when 'getAddrInfo' returns an empty result
  | EstablishTimeout
  -- ^ Occurs on 'socket' connection timeout
  deriving (Show, Exception)

{- |
  Errors intended to be handled by developers
-} 
data UserError
  = UnmatchedType String
  -- ^ Column type mismatch in data packet
  | UnmatchedColumn String
  -- ^ Column name mismatch in data packet
  | UnmatchedColumnsCount String
  -- ^ Occurs when actual columns count less or more than expected
  | DatabaseException ExceptionPacket
  -- ^ Database responded with an exception packet
  deriving (Show, Exception)

{- |
  These exceptions might indicate internal bugs.

  If you encounter one, please report it.
-}
data InternalError
  = UnexpectedPacketType UVarInt
  | DeserializationError String
  deriving (Show, Exception)




-- * Client packets

data ClientPacket where
  Hello                     :: HelloPacket -> ClientPacket
  Query                     :: QueryPacket -> ClientPacket
  Data                      :: DataPacket -> ClientPacket
  Cancel                    :: ClientPacket
  Ping                      :: ClientPacket
  TablesStatusRequest       :: ClientPacket
  KeepAlive                 :: ClientPacket
  Scalar                    :: ClientPacket
  IgnoredPartUUIDs          :: ClientPacket
  ReadTaskResponse          :: ClientPacket
  MergeTreeReadTaskResponse :: ClientPacket
  SSHChallengeRequest       :: ClientPacket
  SSHChallengeResponse      :: ClientPacket
  deriving (Generic)

instance Serializable ClientPacket where
  serialize rev packet = case packet of
    (Hello p)                   -> serialize @UVarInt rev 0 <> serialize rev p
    (Query p)                   -> serialize @UVarInt rev 1 <> serialize rev p
    (Data p)                    -> serialize @UVarInt rev 2 <> serialize rev p
    (Cancel)                    -> serialize @UVarInt rev 3
    (Ping)                      -> serialize @UVarInt rev 4
    (TablesStatusRequest)       -> serialize @UVarInt rev 5
    (KeepAlive)                 -> serialize @UVarInt rev 6
    (Scalar)                    -> serialize @UVarInt rev 7
    (IgnoredPartUUIDs)          -> serialize @UVarInt rev 8
    (ReadTaskResponse)          -> serialize @UVarInt rev 9
    (MergeTreeReadTaskResponse) -> serialize @UVarInt rev 10
    (SSHChallengeRequest)       -> serialize @UVarInt rev 11
    (SSHChallengeResponse)      -> serialize @UVarInt rev 12

-- ** Hello

mkHelloPacket :: Text -> Text -> Text -> ClientPacket
mkHelloPacket db user pass = Hello
  MkHelloPacket
    { client_name          = clientName
    , client_version_major = major
    , client_version_minor = minor
    , tcp_protocol_version = latestSupportedRevision
    , default_database     = toChType db
    , user                 = toChType user
    , pass                 = toChType pass
    }

data HelloPacket = MkHelloPacket
  { client_name          :: ChString
  , client_version_major :: UVarInt
  , client_version_minor :: UVarInt
  , tcp_protocol_version :: ProtocolRevision
  , default_database     :: ChString
  , user                 :: ChString
  , pass                 :: ChString
  }
  deriving (Generic, Serializable)


data Addendum = MkAddendum{quota_key :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY}
  deriving (Generic, Serializable)

-- ** Query

mkQueryPacket :: ConnectionState -> ChString -> ClientPacket
mkQueryPacket MkConnectionState{..} query = Query
  MkQueryPacket
  { query_id = ""
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
  { query_id           :: ChString
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

mkDataPacket :: ChString -> UVarInt -> UVarInt -> ClientPacket
mkDataPacket table_name columns_count rows_count = Data
  MkDataPacket
    { table_name
    , block_info    = MkBlockInfo
      { field_num1   = 1, is_overflows = 0
      , field_num2   = 2, bucket_num   = -1
      , eof          = 0
      }
    , columns_count
    , rows_count
    }

data DataPacket = MkDataPacket
  { table_name    :: ChString
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

data ServerPacket where
  HelloResponse        :: HelloResponse -> ServerPacket
  DataResponse         :: DataPacket -> ServerPacket
  Exception            :: ExceptionPacket -> ServerPacket
  Progress             :: ProgressPacket -> ServerPacket
  Pong                 :: ServerPacket
  EndOfStream          :: ServerPacket
  ProfileInfo          :: ProfileInfo -> ServerPacket
  Totals               :: ServerPacket
  Extremes             :: ServerPacket
  TablesStatusResponse :: ServerPacket
  Log                  :: ServerPacket
  TableColumns         :: TableColumns -> ServerPacket
  UUIDs                :: ServerPacket
  ReadTaskRequest      :: ServerPacket
  ProfileEvents        :: ServerPacket
  UnknownPacket        :: UVarInt -> ServerPacket

instance Deserializable ServerPacket where
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

serverPacketToNum :: ServerPacket -> UVarInt
serverPacketToNum = \case
  (HelloResponse _) -> 0; (DataResponse _)       -> 1
  (Exception _)     -> 2; (Progress _)           -> 3;
  (Pong)            -> 4; (EndOfStream)          -> 5
  (ProfileInfo _)   -> 6; (Totals)               -> 7
  (Extremes)        -> 8; (TablesStatusResponse) -> 9
  (Log)             -> 10; (TableColumns _)      -> 11;
  (UUIDs)           -> 12; (ReadTaskRequest)     -> 13
  (ProfileEvents)   -> 14; (UnknownPacket num)   -> num


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

data ExceptionPacket = MkExceptionPacket
  { code        :: Int32
  , name        :: ChString
  , message     :: ChString
  , stack_trace :: ChString
  , nested      :: UInt8
  }
  deriving (Generic, Show, Deserializable)

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

data TableColumns = MkTableColumns
  { table_name :: ChString
  , table_columns :: ChString
  }
  deriving (Generic, Deserializable)








-- * Deserialization

-- ** Generic API

type GenericClickHaskell record hasColumns =
  ( Generic record
  , GClickHaskell hasColumns (Rep record)
  )

class ClickHaskell columns record
  where
  default deserializeColumns :: GenericClickHaskell record columns => Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns :: Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns isCheckRequired rev size = (to <$!>) <$> gFromColumns @columns isCheckRequired rev size

  default columns :: GenericClickHaskell record columns => Builder
  columns :: Builder
  columns = buildCols (gReadingColumns @columns @(Rep record))
    where
    buildCols [] = mempty
    buildCols ((col, _):[])   = col
    buildCols ((col, _):rest) = col <> ", " <> buildCols rest

  default readingColumnsAndTypes :: GenericClickHaskell record columns => Builder
  readingColumnsAndTypes ::  Builder
  readingColumnsAndTypes = buildColsTypes (gReadingColumns @columns @(Rep record))
    where
    buildColsTypes [] = mempty
    buildColsTypes ((col, typ):[])   = col <> " " <> typ
    buildColsTypes ((col, typ):rest) = col <> " " <> typ <> ", " <> buildColsTypes rest

  default serializeRecords :: GenericClickHaskell record columns => [record] -> ProtocolRevision -> Builder
  serializeRecords :: [record] -> ProtocolRevision -> Builder
  serializeRecords records rev = gSerializeRecords @columns rev (from <$!> records)

  default columnsCount :: GenericClickHaskell record columns => UVarInt
  columnsCount :: UVarInt
  columnsCount = gColumnsCount @columns @(Rep record)

class GClickHaskell (columns :: [Type]) f
  where
  gFromColumns :: Bool -> ProtocolRevision -> UVarInt -> Get [f p]
  gReadingColumns :: [(Builder, Builder)]
  gSerializeRecords :: ProtocolRevision -> [f p] -> Builder
  gColumnsCount :: UVarInt

instance
  GClickHaskell columns f
  =>
  GClickHaskell columns (D1 c (C1 c2 f))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns isCheckRequired rev size = map (M1 . M1) <$> gFromColumns @columns isCheckRequired rev size
  gReadingColumns = gReadingColumns @columns @f
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = gSerializeRecords @columns rev . map (unM1 . unM1)
  gColumnsCount = gColumnsCount @columns @f

instance
  (GClickHaskell columns left, GClickHaskell columns right)
  =>
  GClickHaskell columns (left :*: right)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns isCheckRequired rev size = do
    liftA2 (zipWith (:*:))
      (gFromColumns @columns @left isCheckRequired rev size)
      (gFromColumns @columns @right isCheckRequired rev size)
  gReadingColumns = gReadingColumns @columns @left ++ gReadingColumns @columns @right
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs =
    (\(ls,rs) -> gSerializeRecords @columns rev ls <> gSerializeRecords @columns rev rs)
      (foldr (\(l :*: r) (accL, accR) -> (l:accL, r:accR)) ([], []) xs)
  gColumnsCount = gColumnsCount @columns @left + gColumnsCount @columns @right


instance
  ( KnownColumn (Column name chType)
  , SerializableColumn (Column name chType)
  , FromChType chType inputType
  , ToChType chType inputType
  , Column name chType ~ TakeColumn name columns
  ) => GClickHaskell columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns isCheckRequired rev size =
    map (M1 . K1 . fromChType @chType) . columnValues
      <$> deserializeColumn @(Column name chType) rev isCheckRequired size
  gReadingColumns = (renderColumnName @(Column name chType), renderColumnType @(Column name chType)) : []
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = serializeColumn rev . mkColumn @(Column name chType) . map (toChType . unK1 . unM1)
  gColumnsCount = 1


type family
  TakeColumn (name :: Symbol) (columns :: [Type]) :: Type
  where
  TakeColumn name columns = GoTakeColumn name columns '[]

type family
  GoTakeColumn name (columns :: [Type]) (acc :: [Type]) :: Type
  where
  GoTakeColumn name (Column name chType ': columns) acc = Column name chType
  GoTakeColumn name (Column name1 chType ': columns) acc = (GoTakeColumn name columns (Column name1 chType ': acc))
  GoTakeColumn name '[]                 acc = TypeError
    (    'Text "There is no column \"" :<>: 'Text name :<>: 'Text "\" in table"
    :$$: 'Text "You can't use this field"
    )


-- ** Column deserialization

{-# SPECIALIZE replicateM :: Int -> Get chType -> Get [chType] #-}

class SerializableColumn column where
  deserializeColumn :: ProtocolRevision -> Bool -> UVarInt -> Get column
  serializeColumn :: ProtocolRevision -> column -> Builder

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

  _isCustom <- deserialize @(UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
  pure ()

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name chType) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired rows = do
    handleColumnHeader @(Column name chType) rev isCheckRequired
    mkColumn @(Column name chType)
      <$> replicateM (fromIntegral rows) (deserialize @chType rev)

  {-# INLINE serializeColumn #-}
  serializeColumn rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Deserializable chType
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name (Nullable chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired rows = do
    handleColumnHeader @(Column name (Nullable chType)) rev isCheckRequired
    nulls <- replicateM (fromIntegral rows) (deserialize @UInt8 rev)
    mkColumn @(Column name (Nullable chType)) <$>
      forM
        nulls
        (\case
          0 -> Just <$> deserialize @chType rev
          _ -> (Nothing <$ deserialize @chType rev)
        )

  {-# INLINE serializeColumn #-}
  serializeColumn rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    -- Nulls
    <> mconcat (Prelude.map (serialize @UInt8 rev . maybe 1 (const 0)) (columnValues column))
    -- Values
    <> mconcat (Prelude.map (serialize @chType rev . maybe defaultValueOfTypeName id) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (LowCardinality chType))
  , Deserializable chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  SerializableColumn (Column name (LowCardinality chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired rows = do
    handleColumnHeader @(Column name (LowCardinality chType)) rev isCheckRequired
    _serializationType <- (.&. 0xf) <$> deserialize @UInt64 rev
    _index_size <- deserialize @Int64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    mkColumn @(Column name (LowCardinality chType))
      <$> replicateM (fromIntegral rows) (toChType <$> deserialize @chType rev)

  {-# INLINE serializeColumn #-}
  serializeColumn rev (LowCardinalityColumn column)
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    <> undefined column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Array chType))
  , Deserializable chType
  , TypeError ('Text "Arrays deserialization still unsupported")
  )
  => SerializableColumn (Column name (Array chType)) where
  {-# INLINE deserializeColumn #-}
  deserializeColumn rev isCheckRequired _rows = do
    handleColumnHeader @(Column name (Array chType)) rev isCheckRequired
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

  {-# INLINE serializeColumn #-}
  serializeColumn _rev _column = undefined

-- ** Generics

class
  Deserializable chType
  where
  {-# INLINE deserialize #-}
  default deserialize :: (Generic chType, GDeserial (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev

class GDeserial f
  where
  gDeserialize :: ProtocolRevision -> Get (f p)

instance GDeserial f => GDeserial (D1 c (C1 c2 f))
  where
  gDeserialize rev = M1 . M1 <$> gDeserialize rev
  {-# INLINE gDeserialize #-}

instance (GDeserial left, GDeserial right) => GDeserial (left :*: right) where
  gDeserialize rev = do
    liftA2 (:*:)
      (gDeserialize rev)
      (gDeserialize rev)
  {-# INLINE gDeserialize #-}

instance {-# OVERLAPPING #-}
  GDeserial right => GDeserial (S1 metaSel (Rec0 ProtocolRevision) :*: right) where
  gDeserialize rev = do
    chosenRev <- min rev . coerce <$> deserialize @UVarInt rev
    liftA2 (:*:)
      (pure . M1 . K1 $ chosenRev)
      (gDeserialize @right chosenRev)
  {-# INLINE gDeserialize #-}

instance
  Deserializable chType
  =>
  GDeserial (S1 metaSel (Rec0 chType))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = M1 . K1 <$> deserialize @chType rev 


-- ** Database types

instance Deserializable Int8 where deserialize _ = toChType <$> getInt8; {-# INLINE deserialize #-}
instance Deserializable Int16 where deserialize _ = toChType <$> getInt16le; {-# INLINE deserialize #-}
instance Deserializable Int32 where deserialize _ = toChType <$> getInt32le; {-# INLINE deserialize #-}
instance Deserializable Int64 where deserialize _ = toChType <$> getInt64le; {-# INLINE deserialize #-}
instance Deserializable Int128 where
  deserialize _ = do
    liftA2 (flip Int128)
      getWord64le
      getWord64le
  {-# INLINE deserialize #-}
instance Deserializable UInt8 where deserialize _ = toChType <$> getWord8; {-# INLINE deserialize #-}
instance Deserializable UInt16 where deserialize _ = toChType <$> getWord16le; {-# INLINE deserialize #-}
instance Deserializable UInt32 where deserialize _ = toChType <$> getWord32le; {-# INLINE deserialize #-}
instance Deserializable UInt64 where deserialize _ = toChType <$> getWord64le; {-# INLINE deserialize #-}
instance Deserializable UInt128 where
  deserialize _ = do
    liftA2 (flip Word128)
      getWord64le
      getWord64le
  {-# INLINE deserialize #-}
instance Deserializable UUID where
  deserialize _ = do
      MkChUUID <$> liftA2 (flip Word128)
        getWord64le
        getWord64le
  {-# INLINE deserialize #-}
instance Deserializable Date where deserialize _ = toChType <$> getWord16le; {-# INLINE deserialize #-}
instance Deserializable (DateTime tz) where deserialize _ = toChType <$> getWord32le; {-# INLINE deserialize #-}
instance Deserializable (DateTime64 precision tz) where deserialize _ = toChType <$> getWord64le; {-# INLINE deserialize #-}
instance Deserializable ChString where
  {-# INLINE deserialize #-}
  deserialize = fmap toChType . getByteString . fromIntegral <=< deserialize @UVarInt
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
instance FromChType (DateTime64 precision tz) Word64 where fromChType = coerce
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








-- * Column

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
  DateTime64Column :: [DateTime64 precision tz] -> Column name (DateTime64 precision tz)
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
  (DateColumn values) -> values; (DateTimeColumn values) -> values; (DateTime64Column values) -> values;
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
  renderColumnType = byteString . BS8.pack $ chTypeName @(GetColumnType column)

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
instance
  ( KnownSymbol name
  , IsChType (DateTime64 precision tz)
  ) =>
  KnownColumn (Column name (DateTime64 precision tz)) where mkColumn = DateTime64Column
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

    
class ToQueryPart chType where toQueryPart :: chType -> Builder
instance ToQueryPart Int8 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int16 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int32 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int64 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int128 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt8 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt16 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt32 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt64 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt128 where toQueryPart = byteString . BS8.pack . show
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
    escapeQuery = byteString . BS8.concatMap (\case '\'' -> "\\\'"; '\\' -> "\\\\"; sym -> singleton sym;)

-- ToDo: Need to be fixed
-- instance ToQueryPart (DateTime64 precision tz)
--   where
--   toQueryPart chDateTime =
--     let time = BS8.pack . show . fromChType @_ @Word64 $ chDateTime
--     in byteString (BS8.replicate (12 - BS8.length time) '0' <> time)

instance ToQueryPart (DateTime tz)
  where
  toQueryPart chDateTime = let time = BS8.pack . show . fromChType @(DateTime tz) @Word32 $ chDateTime
    in byteString (BS8.replicate (10 - BS8.length time) '0' <> time)
instance (IsChType chType, ToQueryPart chType) => ToQueryPart (Array chType)
  where
  toQueryPart
    = (\x -> "[" <> x <> "]")
    . (maybe "" (uncurry (foldr (\a b -> a <> "," <> b))) . uncons
    . map (toQueryPart @chType)) . fromChType @(Array chType) @[chType]








-- * Serialization

-- *** Generic API

class Serializable chType
  where
  default serialize :: (Generic chType, GSerial (Rep chType)) => ProtocolRevision -> chType -> Builder
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
instance Serializable (DateTime64 precision tz) where serialize _ = word64LE . fromChType
instance Serializable Date where serialize _ = word16LE . fromChType


-- ** Generics

class GSerial f where
  gSerialize :: ProtocolRevision -> f p -> Builder

instance GSerial f => GSerial (D1 c (C1 c2 f)) where
  gSerialize rev (M1 (M1 re)) = gSerialize rev re
  {-# INLINE gSerialize #-}

instance (GSerial left1,  GSerial right) => GSerial (left1 :*: right) where
  gSerialize rev (l :*: r) = gSerialize rev l <> gSerialize rev r
  {-# INLINE gSerialize #-}

instance Serializable chType => GSerial (S1 metaSel (Rec0 chType)) where
  gSerialize rev (M1 (K1 re)) = serialize rev re
  {-# INLINE gSerialize #-}


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
instance ToChType (DateTime64 precision tz) Word64 where toChType = MkDateTime64
instance ToChType Date Word16 where toChType = MkChDate
instance ToChType chType inputType => ToChType (Array chType) [inputType]
  where
  toChType = MkChArray . map toChType








class IsChType chType
  where
  -- | Shows database original type name
  --
  -- @
  -- chTypeName \@ChString = \"String\"
  -- chTypeName \@(Nullable UInt32) = \"Nullable(UInt32)\"
  -- @
  chTypeName :: String

  defaultValueOfTypeName :: chType

instance IsChType Int8 where; chTypeName = "Int8"; defaultValueOfTypeName = 0
instance IsChType Int16 where; chTypeName = "Int16"; defaultValueOfTypeName = 0
instance IsChType Int32 where; chTypeName = "Int32"; defaultValueOfTypeName = 0
instance IsChType Int64 where; chTypeName = "Int64"; defaultValueOfTypeName = 0
instance IsChType Int128 where; chTypeName = "Int128"; defaultValueOfTypeName = 0

{- | ClickHouse UInt8 column type -}
type UInt8 = Word8
instance IsChType UInt8 where; chTypeName = "UInt8"; defaultValueOfTypeName = 0

{- | ClickHouse UInt16 column type -}
type UInt16 = Word16
instance IsChType UInt16 where; chTypeName = "UInt16"; defaultValueOfTypeName = 0

{- | ClickHouse UInt32 column type -}
type UInt32 = Word32
instance IsChType UInt32 where; chTypeName = "UInt32"; defaultValueOfTypeName = 0

{- | ClickHouse UInt64 column type -}
type UInt64 = Word64
instance IsChType UInt64 where; chTypeName = "UInt64"; defaultValueOfTypeName = 0

{- | ClickHouse UInt128 column type -}
type UInt128 = Word128
instance IsChType UInt128 where; chTypeName = "UInt128"; defaultValueOfTypeName = 0

{- | ClickHouse Date column type -}
newtype Date = MkChDate Word16
  deriving newtype (Show, Eq, Bits, Bounded, Enum, NFData, Num)
instance IsChType Date where; chTypeName = "Date"; defaultValueOfTypeName = 0

{- | ClickHouse String column type -}
newtype ChString = MkChString BS.ByteString
  deriving newtype (Show, Eq, IsString, NFData)
instance IsChType ChString where; chTypeName = "String"; defaultValueOfTypeName = ""

{- | ClickHouse UUID column type -}
newtype UUID = MkChUUID Word128
  deriving newtype (Generic, Show, Eq, NFData, Bounded, Enum, Num)
instance IsChType UUID where; chTypeName = "UUID"; defaultValueOfTypeName = 0

{- | ClickHouse Nullable(T) column type
 (type synonym for Maybe)
 -}
type Nullable = Maybe
instance IsChType chType => IsChType (Nullable chType)
  where
  chTypeName = "Nullable(" <> chTypeName @chType <> ")"
  defaultValueOfTypeName = Nothing

{- |
ClickHouse DateTime column type (paramtrized with timezone)

>>> chTypeName @(DateTime "")
"DateTime"
>>> chTypeName @(DateTime "UTC")
"DateTime('UTC')"
-}
newtype DateTime (tz :: Symbol) = MkDateTime Word32
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance KnownSymbol tz => IsChType (DateTime tz)
  where
  chTypeName = case (symbolVal @tz Proxy) of
    "" -> "DateTime"
    tz -> "DateTime('" <> tz <> "')" 
  defaultValueOfTypeName = MkDateTime 0

{- |
ClickHouse DateTime64 column type (paramtrized with timezone)

>>> chTypeName @(DateTime64 3 "")
"DateTime64(3)"
>>> chTypeName @(DateTime64 3 "UTC")
"DateTime64(3, 'UTC')"
-}
newtype DateTime64 (precision :: Nat) (tz :: Symbol) = MkDateTime64 Word64
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance
  (KnownSymbol tz, KnownNat precision)
  =>
  IsChType (DateTime64 precision tz)
  where
  chTypeName =
    let
      prec = show (natVal @precision Proxy)
    in
    case symbolVal @tz Proxy of
      "" -> "DateTime64(" <> prec <> ")"
      tz -> "DateTime64(" <> prec <> ", '" <> tz <> "')"
  defaultValueOfTypeName = MkDateTime64 0


-- | ClickHouse Array column type
newtype Array a = MkChArray [a]
  deriving newtype (Show, Eq, NFData)
instance IsChType chType => IsChType (Array chType)
  where
  chTypeName = "Array(" <> chTypeName @chType <> ")"
  defaultValueOfTypeName = MkChArray []

-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
instance IsLowCardinalitySupported chType => IsChType (LowCardinality chType)
  where
  chTypeName = "LowCardinality(" <> chTypeName @chType <> ")"
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








-- * Protocol parts

{- |
  Unsigned variable-length quantity encoding
  
  Part of protocol implementation
-}
newtype UVarInt = MkUVarInt Word64
  deriving newtype (Show, Eq, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)


major, minor, patch :: UVarInt
major = case versionBranch version of (x:_) -> fromIntegral x; _ -> 0
minor = case versionBranch version of (_:x:_) -> fromIntegral x; _ -> 0
patch = case versionBranch version of (_:_:x:_) -> fromIntegral x; _ -> 0

clientName :: ChString
clientName = fromString $
  "ClickHaskell-" <> show major <> "." <> show minor <> "." <> show patch

newtype ProtocolRevision = MkProtocolRevision UVarInt
  deriving newtype (Eq, Num, Ord, Serializable)

{-# INLINE [0] afterRevision #-}
afterRevision
  :: forall rev monoid
  . (KnownNat rev, Monoid monoid)
  => ProtocolRevision -> monoid -> monoid
afterRevision chosenRevision monoid =
  if chosenRevision >= (fromIntegral . natVal) (Proxy @rev)
  then monoid
  else mempty

latestSupportedRevision :: ProtocolRevision
latestSupportedRevision = (fromIntegral . natVal) (Proxy @DBMS_TCP_PROTOCOL_VERSION)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented

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
-- type DBMS_MIN_REVISION_WITH_TABLES_STATUS = 54226;
-- type DBMS_MIN_REVISION_WITH_TIME_ZONE_PARAMETER_IN_DATETIME_DATA_TYPE = 54337;
type DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME = 54372;
type DBMS_MIN_REVISION_WITH_VERSION_PATCH = 54401;
-- type DBMS_MIN_REVISION_WITH_SERVER_LOGS = 54406;
-- type DBMS_MIN_REVISION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 54448;
-- type DBMS_MIN_MAJOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 21;
-- type DBMS_MIN_MINOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 4;
-- type DBMS_MIN_REVISION_WITH_COLUMN_DEFAULTS_METADATA = 54410;
-- type DBMS_MIN_REVISION_WITH_LOW_CARDINALITY_TYPE = 54405;
type DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO = 54420;
-- type DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS = 54429;
-- type DBMS_MIN_REVISION_WITH_SCALARS = 54429;
type DBMS_MIN_REVISION_WITH_OPENTELEMETRY = 54442;
-- type DBMS_MIN_REVISION_WITH_AGGREGATE_FUNCTIONS_VERSIONING = 54452;
-- type DBMS_CLUSTER_PROCESSING_PROTOCOL_VERSION = 1;
-- type DBMS_MIN_SUPPORTED_PARALLEL_REPLICAS_PROTOCOL_VERSION = 3;
-- type DBMS_PARALLEL_REPLICAS_MIN_VERSION_WITH_MARK_SEGMENT_SIZE_FIELD = 4;
-- type DBMS_PARALLEL_REPLICAS_PROTOCOL_VERSION = 4;
type DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS = 54453;
-- type DBMS_MERGE_TREE_PART_INFO_VERSION = 1;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET = 54441;
-- type DBMS_MIN_REVISION_WITH_X_FORWARDED_FOR_IN_CLIENT_INFO = 54443;
-- type DBMS_MIN_REVISION_WITH_REFERER_IN_CLIENT_INFO = 54447;
type DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH = 54448;
-- type DBMS_MIN_PROTOCOL_VERSION_WITH_INCREMENTAL_PROFILE_EVENTS = 54451;
type DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION = 54454;
type DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME = 54449;
-- type DBMS_MIN_PROTOCOL_VERSION_WITH_PROFILE_EVENTS_IN_INSERT = 54456;
-- type DBMS_MIN_PROTOCOL_VERSION_WITH_VIEW_IF_PERMITTED = 54457;
-- type DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS = 54459;
-- type DBMS_MIN_PROTOCOL_VERSION_WITH_SERVER_QUERY_TIME_IN_PROGRESS = 54460;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES = 54461;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2 = 54462;
type DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS = 54463;
-- type DBMS_MIN_PROTOCOL_VERSION_WITH_TIMEZONE_UPDATES = 54464;
-- type DBMS_MIN_REVISION_WITH_SPARSE_SERIALIZATION = 54465;
-- type DBMS_MIN_REVISION_WITH_SSH_AUTHENTICATION = 54466;
-- type DBMS_MIN_REVISION_WITH_TABLE_READ_ONLY_CHECK = 54467;
-- type DBMS_MIN_REVISION_WITH_SYSTEM_KEYWORDS_TABLE = 54468;
type DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION = 54469;
type DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS = 54470;
type DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL = 54471;
