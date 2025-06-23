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
import ClickHaskell.Columns
import ClickHaskell.Packets
import ClickHaskell.Primitive

-- GHC included
import Control.Applicative (liftA2)
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, mask, onException, throw, throwIO)
import Control.Monad (when, (<$!>))
import Data.Binary.Get
import Data.Bits (Bits (unsafeShiftR))
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
import Data.Text (Text)
import Data.Text.Encoding as Text (encodeUtf8)
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..))
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (C1, D1, Generic (..), K1 (K1, unK1), M1 (M1, unM1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Prelude hiding (liftA2)
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
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)

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
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


-- * Ping

ping :: HasCallStack => Connection -> IO ()
ping conn = do
  withConnection conn $ \connState@MkConnectionState{revision, buffer} -> do
    writeToConnection connState Ping
    responsePacket <- rawBufferizedRead buffer (deserialize revision)
    case responsePacket of
      Pong                -> pure ()
      Exception exception -> throwIO (DatabaseException exception)
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
          (throw . UnmatchedResult . UnmatchedColumnsCount)
            ("Expected " <> show expected <> " columns but got " <> show columns_count)
        result <- f =<< rawBufferizedRead buffer (deserializeColumns @columns True revision rows_count)
        loop (result : acc)
      Progress    _       -> loop acc
      ProfileInfo _       -> loop acc
      EndOfStream         -> pure acc
      Exception exception -> throwIO (DatabaseException exception)
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
    Exception exception -> throwIO (DatabaseException exception)
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








-- * Errors handling

{- |
  A wrapper for all client-related errors
-}
data ClientError where
  UnmatchedResult :: HasCallStack => UserError -> ClientError
  DatabaseException :: HasCallStack => ExceptionPacket -> ClientError
    -- ^ Database responded with an exception packet
  InternalError :: HasCallStack => InternalError -> ClientError
  deriving anyclass (Exception)

instance Show ClientError where
  show (UnmatchedResult err) = "UserError " <> show err <> "\n" <> prettyCallStack callStack
  show (DatabaseException err) = "DatabaseException " <> show err <> "\n" <> prettyCallStack callStack
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
  These exceptions might indicate internal bugs.

  If you encounter one, please report it.
-}
data InternalError
  = UnexpectedPacketType UVarInt
  | DeserializationError String
  deriving (Show, Exception)








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
    either throw (map (M1 . K1 . fromChType @chType) . columnValues)
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


-- ** FromChType

class FromChType chType outputType where fromChType  :: chType -> outputType

instance FromChType UUID (Word64, Word64) where fromChType (MkUUID (Word128 w64hi w64lo)) = (w64hi, w64lo)
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
  toQueryPart (MkUUID (Word128 hi lo)) = mconcat
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
instance ToChType UUID Word64 where toChType = MkUUID . flip Word128 0
instance ToChType UUID (Word64, Word64) where toChType = MkUUID . uncurry (flip Word128)
instance ToChType (DateTime tz) Word32     where toChType = MkDateTime
instance ToChType (DateTime tz) UTCTime    where toChType = MkDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType (DateTime tz) ZonedTime  where toChType = MkDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC
instance ToChType (DateTime64 precision tz) Word64 where toChType = MkDateTime64
instance ToChType Date Word16 where toChType = MkDate
instance ToChType chType inputType => ToChType (Array chType) [inputType]
  where
  toChType = MkChArray . map toChType
