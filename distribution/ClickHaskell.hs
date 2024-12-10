{-# LANGUAGE
    BangPatterns
  , ConstraintKinds
  , LambdaCase
  , NumericUnderscores
  , OverloadedStrings
  , RecordWildCards
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
  , selectFromView, View, parameter, Parameter
  -- *** Streaming
  , streamSelect
  , streamSelectFrom
  , streamSelectFromView
  -- *** Internal
  , handleSelect

  -- ** Writing
  , WritableInto(..)
  , insertInto

  -- * Ping database connection
  , ping
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol
  ( mkDataPacket, DataPacket(..)
  , mkHelloPacket, HelloParameters(..), mkAddendum
  , mkPingPacket
  , mkQueryPacket
  , ServerPacketType(..), HelloResponse(..), ExceptionPacket, latestSupportedRevision
  )
import ClickHaskell.Columns
  ( HasColumns (..), WritableInto (..), ReadableFrom (..)
  , Columns, DeserializableColumns (..)
  , Column, DeserializableColumn(..), KnownColumn(..)
  )
import ClickHaskell.Parameters (Parameter, parameter, parameters, Parameters, CheckParameters)
import ClickHaskell.DeSerialization (Serializable(..), Deserializable(..), ProtocolRevision)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throwIO)
import Control.DeepSeq (NFData, (<$!!>))
import Data.Binary.Get (Decoder (..), Get, runGetIncremental)
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Char8 as BS8 (fromStrict, pack)
import Data.ByteString.Lazy.Internal as BL (ByteString (..), LazyByteString)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Typeable (Proxy (..))
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

defaultCredentials :: ChCredential
defaultCredentials = MkChCredential
  { chLogin    = "default"
  , chPass     = ""
  , chHost     = "localhost"
  , chDatabase = "default"
  , chPort     = "9000"
  }

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
ping MkConnection{sock, revision, bufferSize} = do
  (sendAll sock . toLazyByteString)
    (serialize revision mkPingPacket)
  (responsePacket, _) <- rawBufferizedRead emptyBuffer (deserialize revision) sock bufferSize
  case responsePacket of
    Pong                -> pure ()
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError . UnexpectedPacketType $ otherPacket)




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
selectFrom conn@MkConnection{sock, user, revision} = do
  let query
        = "SELECT " <> readingColumns @table @record
        <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  handleSelect @table conn emptyBuffer pure

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
  handleSelect @(Columns columns) conn emptyBuffer pure

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
selectFromView conn@MkConnection{..} interpreter = do
  let query =
        "SELECT " <> readingColumns @view @record <>
        " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> parameters interpreter
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  handleSelect @view conn emptyBuffer pure

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
streamSelectFrom conn@MkConnection{sock, user, revision} f = do
  let query
        = "SELECT " <> readingColumns @table @record
        <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  let f' x = id <$!!> f x
  handleSelect @table conn emptyBuffer f'

streamSelect ::
  forall columns record a
  .
  (ReadableFrom (Columns columns) record, NFData a)
  =>
  Connection -> ChString -> ([record] -> IO [a]) -> IO [a]
streamSelect conn@MkConnection{sock, user, revision} query f = do
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user query)
    <> serialize revision (mkDataPacket "" 0 0)
    )
  let f' x = id <$!!> f x
  handleSelect @(Columns columns) conn emptyBuffer f'

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
streamSelectFromView conn@MkConnection{..} interpreter f = do
  let query =
        "SELECT " <> readingColumns @view @record <>
        " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> parameters interpreter
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  let f' x = id <$!!> f x
  handleSelect @view conn emptyBuffer f'

-- *** Internal

handleSelect ::
  forall hasColumns record a
  .
  ReadableFrom hasColumns record
  =>
  Connection -> Buffer -> ([record] -> IO [a])  -> IO [a]
handleSelect conn@MkConnection{..} previousBuffer f = do
  (packet, buffer) <- rawBufferizedRead previousBuffer (deserialize revision) sock bufferSize  
  case packet of
    DataResponse MkDataPacket{columns_count, rows_count} -> do
      case (columns_count, rows_count) of
        (0, 0) -> handleSelect @hasColumns conn buffer f
        (_, rows) -> do
          (columns, nextBuffer) <- rawBufferizedRead buffer (deserializeColumns @hasColumns revision rows) sock bufferSize
          processedColumns <- f columns
          (processedColumns ++) <$> handleSelect @hasColumns conn nextBuffer f 
    Progress          _ -> handleSelect @hasColumns conn buffer f
    ProfileInfo       _ -> handleSelect @hasColumns conn buffer f
    EndOfStream         -> pure []
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)


-- ** Inserting

insertInto ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , WritableInto table record
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
  handleInsertResult @table conn emptyBuffer columnsData

handleInsertResult :: forall columns record . WritableInto columns record => Connection -> Buffer -> [record] -> IO ()
handleInsertResult conn@MkConnection{..} buffer records = do
  (firstPacket, buffer1) <- rawBufferizedRead buffer (deserialize revision) sock bufferSize
  case firstPacket of
    TableColumns      _ -> handleInsertResult @columns conn buffer1 records
    DataResponse packet -> do
      (_emptyDataPacket, buffer2)
        <- rawBufferizedRead buffer1 (deserializeRawColumns @(Columns (GetColumns columns)) revision (rows_count packet)) sock bufferSize
      (sendAll sock . toLazyByteString)
        (  serialize revision (mkDataPacket "" (columnsCount @columns @record) (fromIntegral $ length records))
        <> serializeRecords @columns revision (fromIntegral $ length records) records
        <> serialize revision (mkDataPacket "" 0 0)
        )
      handleInsertResult @columns @record conn buffer2 []
    EndOfStream         -> pure ()
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)




-- * Bufferization

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
