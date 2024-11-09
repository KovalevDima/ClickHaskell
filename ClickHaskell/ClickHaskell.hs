{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DefaultSignatures
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , LambdaCase
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , PolyKinds
  , RecordWildCards
  , UndecidableInstances
#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module ClickHaskell
  ( module ClickHaskell.DbTypes
  , ChCredential(..)
  , openNativeConnection
  , Table
  , ReadableFrom(..)
  , WritableInto(..)
  , View
  , select
  , selectFrom
  , insertInto
  , ping
  , dev
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol.ClientPackets (DataPacket (..), HelloParameters (..), mkAddendum, mkDataPacket, mkHelloPacket, mkPingPacket, mkQueryPacket)
import ClickHaskell.NativeProtocol.Columns (Column (..), Columns (..), DeserializableColumns (deserializeColumns), HasColumns (..), KnownColumn (..), KnownColumns (..), appendColumn, emptyColumns, mkColumn)
import ClickHaskell.NativeProtocol.Serialization (Deserializable (..), ProtocolRevision, Serializable (..), latestSupportedRevision)
import ClickHaskell.NativeProtocol.ServerPackets (ExceptionPacket, HelloResponse (..), ServerPacketType (..))

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throwIO)
import Control.Monad ((<=<))
import Data.Binary.Get (Decoder (..), Get, runGetIncremental)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8 as BS8 (fromStrict, pack)
import Data.ByteString.Lazy.Internal as BL (ByteString (..), LazyByteString)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Typeable (Proxy (..))
import Data.Word (Word32)
import GHC.Generics
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Timeout (timeout)

-- External
import Network.Socket as Sock
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Debug.Trace (traceShowId)

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
  { sock           :: Socket
  , user           :: ChString
  , bufferSize     :: Int64
  , revision :: ProtocolRevision
  }

openNativeConnection :: HasCallStack => ChCredential -> IO Connection
openNativeConnection MkChCredential{chHost, chPort, chLogin, chPass, chDatabase} = do
  AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress}
    <- (maybe (throwIO $ ConnectionError NoAdressResolved) pure . listToMaybe)
    =<< getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just chHost)
      (Just chPort)
  sock <- (maybe (throwIO $ ConnectionError EstablishTimeout) pure <=< timeout 3_000_000) $
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
    Exception exception -> throwIO . DatabaseException $ exception
    otherPacket         -> throwIO . ProtocolImplementationError $ UnexpectedPacketType otherPacket




-- * Ping

ping :: HasCallStack => Connection -> IO ()
ping conn@MkConnection{sock, revision} = do
  (sendAll sock . toLazyByteString)
    (mkPingPacket revision)
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
    <> serialize revision (mkDataPacket "" emptyColumns)
    )
  handleSelect @table conn emptyBuffer


select ::
  forall columns record
  .
  (ReadableFrom (Columns columns) record, Serializable (Columns columns))
  =>
  Connection -> ChString -> IO [record]
select conn@MkConnection{sock, user, revision} query = do
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user query)
    <> serialize revision (mkDataPacket "" emptyColumns)
    )
  (firstPacket, buffer) <- continueReadDeserializable @ServerPacketType conn emptyBuffer  
  case traceShowId firstPacket of
    DataResponse _      -> do
      (_empty, nextBuffer) <- continueReadColumns @(Columns columns) conn buffer 0
      print (serialize revision _empty)
      handleSelect @(Columns columns) conn nextBuffer
    Exception exception -> throwIO $ DatabaseException exception
    otherPacket         -> throwIO $ ProtocolImplementationError $ UnexpectedPacketType otherPacket

handleSelect :: forall hasColumns record . ReadableFrom hasColumns record => Connection -> Buffer -> IO [record]
handleSelect conn previousBuffer = do
  (packet, buffer) <- continueReadDeserializable @ServerPacketType conn previousBuffer
  case traceShowId packet of
    DataResponse MkDataPacket{rows_count} -> do
      case rows_count of
        0 -> pure []
        rows -> do
          (columns, nextBuffer) <- continueReadColumns @(Columns (GetColumns hasColumns)) conn buffer rows
          ((fromColumns @hasColumns) columns ++) <$> handleSelect @hasColumns conn nextBuffer
    Progress          _ -> handleSelect @hasColumns conn buffer
    ProfileInfo       _ -> handleSelect @hasColumns conn buffer
    EndOfStream         -> pure []
    Exception exception -> throwIO $ DatabaseException exception
    otherPacket         -> throwIO $ ProtocolImplementationError $ UnexpectedPacketType otherPacket

-- ** Inserting

insertInto ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , WritableInto (Table name columns) record
  , KnownSymbol name
  , Serializable (Columns columns)
  )
  => Connection -> [record] -> IO ()
insertInto MkConnection{sock, user, revision} columns = do
  let query =
        "INSERT INTO " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
        <> " (" <> writingColumns @table @record <> ") VALUES"
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" emptyColumns)
    )
  -- ^ answers with 11.TableColumns
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkDataPacket "" . toColumns @(Table name columns) $ columns)
    <> serialize revision (mkDataPacket "" emptyColumns)
    )
  -- print =<< recv sock 4096



-- * Reading

continueReadDeserializable :: Deserializable packet => Connection -> Buffer -> IO (packet, Buffer)
continueReadDeserializable MkConnection{..} buffer = rawBufferizedRead buffer (deserialize revision) sock bufferSize

continueReadColumns :: DeserializableColumns columns => Connection -> Buffer -> UVarInt -> IO (columns, Buffer)
continueReadColumns MkConnection{..} buffer rows = rawBufferizedRead buffer (deserializeColumns revision rows) sock bufferSize

-- ** Bufferization

type Buffer = LazyByteString

emptyBuffer :: Buffer
emptyBuffer = BL.Empty

rawBufferizedRead :: Buffer -> Get packet -> Socket -> Int64 -> IO (packet, LazyByteString)
rawBufferizedRead buffer parser sock bufSize = runBufferReader (recv sock bufSize) (runGetIncremental parser) buffer


runBufferReader :: IO LazyByteString -> Decoder packet -> LazyByteString -> IO (packet, LazyByteString)
runBufferReader bufferFiller (Partial decoder) (BL.Chunk bs mChunk)
  = runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader bufferFiller (Partial decoder) BL.Empty = do
  bufferFiller >>= \case
    BL.Empty -> fail "Expected more bytes while reading packet" -- ToDo: Pass packet name
    BL.Chunk bs mChunk -> runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader _bufferFiller (Done leftover _consumed packet) _input = pure (packet, fromStrict leftover)
runBufferReader _bufferFiller (Fail _leftover _consumed msg) _currentBuffer = error msg




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
  | UnknownPacketType
  | DeserializationError
  deriving (Show, Exception)

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)




-- * Interface

data View (name :: Symbol) (columns :: [Type]) parameters

newtype Table (name :: Symbol) (columns :: [Type]) = MkTable (Columns columns)

instance KnownColumns (Columns columns) => KnownColumns (Table name columns) where
  type ColumnsCount (Table name columns) = ColumnsCount (Columns columns)
  columnsCount = columnsCount @(Columns columns)
  rowsCount (MkTable columns) = rowsCount columns

instance HasColumns (Table name columns)
  where
  type GetColumns (Table _ columns) = columns

-- ** Reading

type GenericReadable record hasColumns =
  ( Generic record
  , GReadable (GetColumns hasColumns) (Rep record)
  )

class
  ( KnownColumns hasColumns
  , HasColumns hasColumns
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
  gFromColumns = map (M1 . M1) . gFromColumns @columns
  gReadingColumns = gReadingColumns @columns @f

instance
  GReadable columns (left :*: (right1 :*: right2))
  =>
  GReadable columns ((left :*: right1) :*: right2)
  where
  gFromColumns rev = (\(l :*: (r1 :*: r2)) -> (l :*: r1) :*: r2) <$> gFromColumns rev
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
  gFromColumns (AddColumn (MkColumn column) extraColumns) =
    zipWith (:*:)
      (map (M1 . K1 . fromChType @chType) column)
      (gFromColumns @restColumns @right extraColumns)
  gReadingColumns =
    renderColumnName @(Column name chType)
    <> ", " <> gReadingColumns @restColumns @right

instance
  ( KnownColumn (Column name chType)
  , FromChType chType inputType
  ) => GReadable '[Column name chType] ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  gFromColumns (AddColumn (MkColumn column) _) = map (M1 . K1 . fromChType @chType) column
  gReadingColumns = renderColumnName @(Column name chType)


-- ** Writing

type GenericWritable record hasColumns =
  ( Generic record
  , GWritable (GetColumns hasColumns) (Rep record)
  )

class
  ( HasColumns hasColumns
  , KnownColumns (Columns (GetColumns hasColumns))
  )
  =>
  WritableInto hasColumns record
  where
  default toColumns :: GenericWritable record hasColumns => [record] -> Columns (GetColumns hasColumns)
  toColumns :: [record] -> Columns (GetColumns hasColumns)
  toColumns = gToColumns @(GetColumns hasColumns) . map from

  default writingColumns :: GenericWritable record hasColumns =>  Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns hasColumns) @(Rep record)


class GWritable columns f
  where
  gToColumns :: [f p] -> Columns columns
  gWritingColumns :: Builder

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  gToColumns = gToColumns @columns . map (unM1 . unM1)
  gWritingColumns = gWritingColumns @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  gToColumns  = gToColumns . map (\((l1 :*: l2) :*: r) -> l1 :*: (l2 :*: r))
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
  gToColumns rows =
    mkColumn (map (\(l :*: _) -> toChType . unK1 . unM1 $ l) rows)
    `appendColumn`
    (gToColumns @restColumns $ map (\(_ :*: r) -> r) rows)
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
  gToColumns rows = (mkColumn . map (toChType . unK1 . unM1)) rows `appendColumn` emptyColumns
  gWritingColumns = renderColumnName @(Column name chType)



-- * Dev

dev :: IO ()
dev = do
  connection <- openNativeConnection devCredential
  print "Connected"
  ping connection
  print "Pinged"
  a <- selectFrom @ExampleTable @ExampleData connection
  print a
  print "Dummy queries done"
  insertInto @ExampleTable @ExampleData connection devColumns

devColumns ::  [ExampleData]
devColumns = MkExample <$> replicate 5 127


type ExampleTable = Table "example" '[Column "val" ChUInt32]

data ExampleData = MkExample
  { val :: Word32
  } deriving (Generic, Show)

instance ReadableFrom ExampleTable ExampleData
instance WritableInto ExampleTable ExampleData


devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
