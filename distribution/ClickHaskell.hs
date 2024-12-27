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

-- Internal dependencies
import ClickHaskell.NativeProtocol
  ( mkDataPacket, DataPacket(..)
  , mkHelloPacket, HelloParameters(..), mkAddendum
  , mkPingPacket
  , mkQueryPacket
  , ServerPacketType(..), HelloResponse(..), ExceptionPacket, latestSupportedRevision
  , HasColumns (..), WritableInto (..), ReadableFrom (..)
  , Columns, DeserializableColumns (..), Column, DeserializableColumn(..), KnownColumn(..)
  , Serializable(..), Deserializable(..), ProtocolRevision
  , Parameter, parameter, Parameters, CheckParameters, viewParameters

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
  )

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throwIO)
import Control.DeepSeq (NFData, (<$!!>))
import Data.Binary.Get (Decoder (..), Get, runGetIncremental)
import Data.ByteString as BS (StrictByteString, length)
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Functor (($>))
import Data.IORef (IORef, readIORef, newIORef, atomicWriteIORef)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Typeable (Proxy (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Timeout (timeout)

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

data Connection = MkConnection
  { sock     :: Socket
  , user     :: ChString
  , buffer   :: Buffer
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

  buffer <- initBuffer 4096 sock
  serverPacketType <- rawBufferizedRead buffer (deserialize latestSupportedRevision)
  case serverPacketType of
    HelloResponse MkHelloResponse{server_revision} -> do
      let revision = min server_revision latestSupportedRevision
      (sendAll sock . toLazyByteString) (serialize revision mkAddendum)
      pure MkConnection
        { user = toChType chLogin
        , revision
        , sock
        , buffer
        }
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)


-- * Ping

ping :: HasCallStack => Connection -> IO ()
ping MkConnection{sock, revision, buffer} = do
  (sendAll sock . toLazyByteString)
    (serialize revision mkPingPacket)
  responsePacket <- rawBufferizedRead buffer (deserialize revision)
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
  handleSelect @table conn pure

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
  handleSelect @(Columns columns) conn pure

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
        " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> viewParameters interpreter
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  handleSelect @view conn pure

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
  handleSelect @table conn f'

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
  handleSelect @(Columns columns) conn f'

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
        " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> viewParameters interpreter
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  let f' x = id <$!!> f x
  handleSelect @view conn f'

-- *** Internal

handleSelect ::
  forall hasColumns record a
  .
  ReadableFrom hasColumns record
  =>
  Connection -> ([record] -> IO [a])  -> IO [a]
handleSelect conn@MkConnection{..} f = do
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
  handleInsertResult @table conn columnsData

handleInsertResult :: forall columns record . WritableInto columns record => Connection -> [record] -> IO ()
handleInsertResult conn@MkConnection{..} records = do
  firstPacket <- rawBufferizedRead buffer (deserialize revision)
  case firstPacket of
    TableColumns      _ -> handleInsertResult @columns conn records
    DataResponse (MkDataPacket{rows_count}) -> do
      _emptyDataPacket <- rawBufferizedRead buffer (deserializeRawColumns @(Columns (GetColumns columns)) revision rows_count)
      (sendAll sock . toLazyByteString)
        (  serialize revision (mkDataPacket "" (columnsCount @columns @record) (fromIntegral $ Prelude.length records))
        <> serializeRecords @columns revision records
        <> serialize revision (mkDataPacket "" 0 0)
        )
      handleInsertResult @columns @record conn []
    EndOfStream         -> pure ()
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)




-- * Bufferization

data Buffer = MkBuffer
  { bufferSize :: Int
  , bufferSocket :: Socket
  , buff :: IORef StrictByteString
  }

initBuffer :: Int -> Socket -> IO Buffer
initBuffer size sock = MkBuffer size sock <$> newIORef ""


rawBufferizedRead :: Buffer -> Get packet -> IO packet
rawBufferizedRead buffer parser = runBufferReader buffer (runGetIncremental parser)

runBufferReader :: Buffer -> Decoder packet -> IO packet
runBufferReader buffer@MkBuffer{bufferSocket, bufferSize, buff} = \case
  (Partial decoder) -> do
    currentBuffer <- readIORef buff
    chosenBuffer <- case BS.length currentBuffer of
      0 -> recv bufferSocket bufferSize
      _ -> atomicWriteIORef buff "" $> currentBuffer
    case BS.length chosenBuffer of
      0 -> throwIO (DeserializationError "Expected more bytes while reading packet")
      _ -> runBufferReader buffer (decoder $ Just $! chosenBuffer)
  (Done leftover _consumed packet) -> atomicWriteIORef buff leftover $> packet
  (Fail _leftover _consumed msg) -> throwIO (DeserializationError msg)




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
