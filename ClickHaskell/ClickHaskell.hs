{-# LANGUAGE
    ConstraintKinds
  , LambdaCase
  , NumericUnderscores
  , OverloadedStrings
  , RecordWildCards
#-}

module ClickHaskell
  ( module ClickHaskell.DbTypes
  , ChCredential(..)
  , openNativeConnection
  , ReadableFrom(..)
  , WritableInto(..)
  , select
  , selectFrom
  , insertInto
  , ping

  , Columns
  , Table, View
  , parameter, Parameter
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol
  ( mkDataPacket, DataPacket(..)
  , mkHelloPacket, HelloParameters(..), mkAddendum
  , mkPingPacket
  , mkQueryPacket
  , ServerPacketType(..), HelloResponse(..), ExceptionPacket
  , latestSupportedRevision
  )
import ClickHaskell.Columns (Column (..), Columns (..), DeserializableColumns (deserializeColumns), HasColumns (..), KnownColumn (..), KnownColumns (..), appendColumn, emptyColumns, mkColumn)
import ClickHaskell.Parameters (Parameter, parameter)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throwIO)
import Data.Bifunctor (bimap)
import Data.Binary.Get (Decoder (..), Get, runGetIncremental)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8 as BS8 (fromStrict, pack)
import Data.ByteString.Lazy.Internal as BL (ByteString (..), LazyByteString)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Typeable (Proxy (..))
import GHC.Generics
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Timeout (timeout)

-- External
import Network.Socket as Sock
import Network.Socket.ByteString.Lazy (recv, sendAll)

-- * Connection

data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)

data Connection = MkConnection
  { sock       :: Socket
  , user       :: ChString
  , bufferSize :: Int64
  , revision   :: ProtocolRevision
  }

openNativeConnection :: HasCallStack => ChCredential -> IO Connection
openNativeConnection MkChCredential{chHost, chPort, chLogin, chPass, chDatabase} = do
  AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress}
    <- (maybe (throwIO $ ConnectionError NoAdressResolved) pure . listToMaybe)
    =<< getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just chHost)
      (Just chPort)
  sock <- maybe (throwIO $ ConnectionError EstablishTimeout) pure
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

  (serverPacketType, _) <- rawBufferizedRead emptyBuffer (deserialize latestSupportedRevision) sock 4096
  case serverPacketType of
    HelloResponse MkHelloResponse{server_revision} -> do
      let revision = min server_revision latestSupportedRevision
      (sendAll sock . toLazyByteString) (serialize revision mkAddendum)
      pure MkConnection
        { user = toChType chLogin
        , revision
        , sock
        , bufferSize = 4096
        }
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)




-- * Ping

ping :: HasCallStack => Connection -> IO ()
ping conn@MkConnection{sock, revision} = do
  (sendAll sock . toLazyByteString)
    (serialize revision mkPingPacket)
  (responsePacket, _) <- continueReadDeserializable conn emptyBuffer
  case responsePacket of
    Pong                -> pure ()
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError . UnexpectedPacketType $ otherPacket)




-- * Querying

-- ** Selecting

selectFrom ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , KnownSymbol name
  , ReadableFrom table record
  )
  =>
  Connection -> IO [record]
selectFrom conn@MkConnection{sock, user, revision} = do
  let query
        =  "SELECT " <> readingColumns @table @record
        <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  handleSelect @table conn emptyBuffer


select ::
  forall columns record
  .
  ReadableFrom (Columns columns) record
  =>
  Connection -> ChString -> IO [record]
select conn@MkConnection{sock, user, revision} query = do
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user query)
    <> serialize revision (mkDataPacket "" 0 0)
    )
  (firstPacket, buffer) <- continueReadDeserializable @ServerPacketType conn emptyBuffer
  case firstPacket of
    DataResponse _      -> do
      (_empty, nextBuffer) <- continueReadColumns @(Columns columns) conn buffer 0
      handleSelect @(Columns columns) conn nextBuffer
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)

handleSelect :: forall hasColumns record . ReadableFrom hasColumns record => Connection -> Buffer -> IO [record]
handleSelect conn previousBuffer = do
  (packet, buffer) <- continueReadDeserializable @ServerPacketType conn previousBuffer
  case packet of
    DataResponse MkDataPacket{rows_count} -> case rows_count of
      0 -> pure []
      rows -> do
        (columns, nextBuffer) <- continueReadColumns @(Columns (GetColumns hasColumns)) conn buffer rows
        ((fromColumns @hasColumns) columns ++) <$> handleSelect @hasColumns conn nextBuffer
    Progress          _ -> handleSelect @hasColumns conn buffer
    ProfileInfo       _ -> handleSelect @hasColumns conn buffer
    EndOfStream         -> pure []
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)

-- ** Inserting

insertInto ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , WritableInto (Table name columns) record
  , KnownSymbol name
  )
  => Connection -> [record] -> IO ()
insertInto conn@MkConnection{sock, user, revision} columnsData = do
  let query =
        "INSERT INTO " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
        <> " (" <> writingColumns @table @record <> ") VALUES"
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  (firstPacket, buffer1) <- continueReadDeserializable @ServerPacketType conn emptyBuffer
  case firstPacket of
    TableColumns _ ->
      do
      (secondPacket, buffer2) <- continueReadDeserializable @ServerPacketType conn buffer1
      case secondPacket of
        DataResponse (MkDataPacket {rows_count}) ->
          do
          (_emptyDataPacket, _buffer) <- continueReadColumns @(Columns columns) conn buffer2 rows_count
          let columns = toColumns @(Table name columns) (fromIntegral $ length columnsData) columnsData
          (sendAll sock . toLazyByteString)
            (  serialize revision (mkDataPacket "" (columnsCount @(Columns columns)) (rowsCount columns))
            <> serialize revision columns
            <> serialize revision (mkDataPacket "" 0 0)
            )
        Exception exception -> throwIO (DatabaseException exception)
        otherPacket -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)


-- * Reading

continueReadDeserializable :: Deserializable packet => Connection -> Buffer -> IO (packet, Buffer)
continueReadDeserializable MkConnection{..} buffer = rawBufferizedRead buffer (deserialize revision) sock bufferSize

continueReadColumns :: DeserializableColumns columns => Connection -> Buffer -> UVarInt -> IO (columns, Buffer)
continueReadColumns MkConnection{..} buffer rows = rawBufferizedRead buffer (deserializeColumns revision rows) sock bufferSize

-- ** Bufferization

type Buffer = LazyByteString

emptyBuffer :: Buffer
emptyBuffer = BL.Empty

rawBufferizedRead :: Buffer -> Get packet -> Socket -> Int64 -> IO (packet, Buffer)
rawBufferizedRead buffer parser sock bufSize = runBufferReader (recv sock bufSize) (runGetIncremental parser) buffer

runBufferReader :: IO LazyByteString -> Decoder packet -> Buffer -> IO (packet, Buffer)
runBufferReader bufferFiller (Partial decoder) (BL.Chunk bs mChunk)
  = runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader bufferFiller (Partial decoder) BL.Empty = do
  bufferFiller >>= \case
    BL.Empty -> throwIO (DeserializationError "Expected more bytes while reading packet")
    BL.Chunk bs mChunk -> runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader _bufferFiller (Done leftover _consumed packet) _input = pure (packet, fromStrict leftover)
runBufferReader _initBuf (Fail _leftover _consumed msg) _buffer = throwIO (DeserializationError msg)




-- * Errors handling

data ClientError where
  ConnectionError :: HasCallStack => ConnectionError -> ClientError
  DatabaseException :: HasCallStack => ExceptionPacket -> ClientError
  ProtocolImplementationError :: HasCallStack => ProtocolImplementationError -> ClientError

instance Show ClientError where
  show (ConnectionError connError) = "ConnectionError" <> show connError <> "\n" <> prettyCallStack callStack
  show (DatabaseException exception) = "DatabaseException" <> show exception <> "\n" <> prettyCallStack callStack
  show (ProtocolImplementationError err) = "ConnectionError" <> show err <> "\n" <> prettyCallStack callStack

deriving anyclass instance Exception ClientError

{- |
  You shouldn't see this exceptions. Please report a bug if it appears
-}
data ProtocolImplementationError
  = UnexpectedPacketType ServerPacketType
  | DeserializationError String
  deriving (Show, Exception)

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)




-- * Interface

data View (name :: Symbol) (columns :: [Type]) (parameters :: [Type])

data Table (name :: Symbol) (columns :: [Type])


instance HasColumns (Table name columns)
  where
  type GetColumns (Table _ columns) = columns

-- ** Reading

type GenericReadable record hasColumns =
  ( Generic record
  , GReadable (GetColumns hasColumns) (Rep record)
  )

class
  ( HasColumns hasColumns
  , DeserializableColumns (Columns (GetColumns hasColumns))
  ) =>
  ReadableFrom hasColumns record
  where
  default fromColumns :: GenericReadable record hasColumns => Columns (GetColumns hasColumns) -> [record]
  fromColumns :: Columns (GetColumns hasColumns) -> [record]
  fromColumns = map to . gFromColumns @(GetColumns hasColumns)

  default readingColumns :: GenericReadable record hasColumns => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(GetColumns hasColumns) @(Rep record)


class GReadable columns f
  where
  gFromColumns :: Columns columns -> [f p]
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns = map (M1 . M1) . gFromColumns @columns
  gReadingColumns = gReadingColumns @columns @f

instance
  GReadable columns (left :*: (right1 :*: right2))
  =>
  GReadable columns ((left :*: right1) :*: right2)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev = do
    (l :*: (r1 :*: r2)) <- gFromColumns rev
    pure ((l :*: r1) :*: r2)
  gReadingColumns = gReadingColumns @columns @((left :*: right1) :*: right2)

instance
  ( KnownColumn (Column name chType)
  , FromChType chType inputType
  , GReadable restColumns right
  )
  =>
  GReadable
    (Column name chType ': restColumns)
    (S1 (MetaSel (Just name) a b f) (Rec0 inputType) :*: right)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns (AddColumn column extraColumns) =
    zipWith (:*:)
      (gFromColumns (AddColumn column emptyColumns))
      (gFromColumns extraColumns)
  gReadingColumns =
    renderColumnName @(Column name chType)
    <> ", " <> gReadingColumns @restColumns @right

instance
  ( KnownColumn (Column name chType)
  , FromChType chType inputType
  ) => GReadable '[Column name chType] ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns (AddColumn (MkColumn _ column) _) = map (M1 . K1 . fromChType @chType) column
  gReadingColumns = renderColumnName @(Column name chType)


-- ** Writing

type GenericWritable record hasColumns =
  ( Generic record
  , GWritable (GetColumns hasColumns) (Rep record)
  )

class
  ( HasColumns hasColumns
  , KnownColumns (Columns (GetColumns hasColumns))
  , Serializable (Columns (GetColumns hasColumns))
  , DeserializableColumns (Columns (GetColumns hasColumns))
  )
  =>
  WritableInto hasColumns record
  where
  default toColumns :: GenericWritable record hasColumns => UVarInt -> [record] -> Columns (GetColumns hasColumns)
  toColumns :: UVarInt -> [record] -> Columns (GetColumns hasColumns)
  toColumns size = gToColumns @(GetColumns hasColumns) size . map from

  default writingColumns :: GenericWritable record hasColumns =>  Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns hasColumns) @(Rep record)


class GWritable columns f
  where
  gToColumns :: UVarInt -> [f p] -> Columns columns
  gWritingColumns :: Builder

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gToColumns #-}
  gToColumns size = gToColumns @columns size . map (unM1 . unM1)
  {-# INLINE gWritingColumns #-}
  gWritingColumns = gWritingColumns @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  {-# INLINE gToColumns #-}
  gToColumns size = gToColumns size . map (\((l1 :*: l2) :*: r) -> l1 :*: (l2 :*: r))
  {-# INLINE gWritingColumns #-}
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))

instance
  ( GWritable '[Column name chType] (S1 (MetaSel (Just name) a b f) (Rec0 inputType))
  , GWritable restColumns right
  , KnownColumn (Column name chType)
  , ToChType chType inputType
  )
  =>
  GWritable (Column name chType ': restColumns) (S1 (MetaSel (Just name) a b f) (Rec0 inputType) :*: right)
  where
  {-# INLINE gToColumns #-}
  gToColumns size
    = uncurry appendColumn
    . bimap (mkColumn size) (gToColumns @restColumns size)
    . unzip
    . map (\(l :*: r) -> ((toChType . unK1 . unM1) l, r))
     
  {-# INLINE gWritingColumns #-}
  gWritingColumns =
    renderColumnName @(Column name chType)
    <> ", " <> gWritingColumns @restColumns @right

instance
  ( ToChType chType inputType
  , KnownColumn (Column name chType)
  )
  =>
  GWritable '[Column name chType] (S1 (MetaSel (Just name) a b f) (Rec0 inputType))
  where
  {-# INLINE gToColumns #-}
  gToColumns size rows = (MkColumn size . map (toChType . unK1 . unM1)) rows `appendColumn` emptyColumns
  {-# INLINE gWritingColumns #-}
  gWritingColumns = renderColumnName @(Column name chType)
