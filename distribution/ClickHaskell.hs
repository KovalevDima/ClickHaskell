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
import Control.Monad (forM_)
import Data.Binary.Get (Decoder (..), Get, runGetIncremental)
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Char8 as BS8 (fromStrict, pack)
import Data.ByteString.Lazy.Internal as BL (ByteString (..), LazyByteString)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Typeable (Proxy (..))
import GHC.IO (unsafeInterleaveIO)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Timeout (timeout)

-- External
import Data.WideWord (Int128 (..), Word128(..))
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

selectFrom ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , KnownSymbol name
  , ReadableFrom table record
  )
  =>
  Connection -> IO [[record]]
selectFrom conn@MkConnection{sock, user, revision} = do
  let query
        = "SELECT " <> readingColumns @table @record
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
  Connection -> ChString -> IO [[record]]
select conn@MkConnection{sock, user, revision} query = do
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user query)
    <> serialize revision (mkDataPacket "" 0 0)
    )
  handleSelect @(Columns columns) conn emptyBuffer

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
  => Connection -> (Parameters '[] -> Parameters passedParameters) -> IO [[record]]
selectFromView conn@MkConnection{..} interpreter = do
  let query =
        "SELECT " <> readingColumns @view @record <>
        " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> viewParameters interpreter
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  handleSelect @view conn emptyBuffer

-- *** Internal

handleSelect ::
  forall hasColumns record
  .
  ReadableFrom hasColumns record
  =>
  Connection -> Buffer -> IO [[record]]
handleSelect conn@MkConnection{..} previousBuffer = do
  (packet, buffer) <- rawBufferizedRead previousBuffer (deserialize revision) sock bufferSize  
  case packet of
    DataResponse MkDataPacket{columns_count, rows_count} -> do
      case (columns_count, rows_count) of
        (0, 0) -> handleSelect @hasColumns conn buffer
        (_, rows) -> do
          (columns, nextBuffer) <- rawBufferizedRead buffer (deserializeColumns @hasColumns revision rows) sock bufferSize
          unsafeInterleaveIO $ (columns :) <$> handleSelect @hasColumns conn nextBuffer
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
  , WritableInto table record
  , KnownSymbol name
  )
  => Connection -> [[record]] -> IO ()
insertInto conn@MkConnection{sock, user, revision} columnsData = do
  let query =
        "INSERT INTO " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
        <> " (" <> writingColumns @table @record <> ") VALUES"
  (sendAll sock . toLazyByteString)
    (  serialize revision (mkQueryPacket revision user (toChType query))
    <> serialize revision (mkDataPacket "" 0 0)
    )
  handleInsertResult @table conn emptyBuffer columnsData

handleInsertResult :: forall columns record . WritableInto columns record => Connection -> Buffer -> [[record]] -> IO ()
handleInsertResult conn@MkConnection{..} buffer recordsBlocks = do
  (firstPacket, buffer1) <- rawBufferizedRead buffer (deserialize revision) sock bufferSize
  case firstPacket of
    TableColumns _ -> handleInsertResult @columns conn buffer1 recordsBlocks
    DataResponse (MkDataPacket{rows_count}) -> do
      (_emptyDataPacket, buffer2)
        <- rawBufferizedRead buffer1 (deserializeRawColumns @(Columns (GetColumns columns)) revision rows_count) sock bufferSize
      forM_ recordsBlocks $ \records ->
        (sendAll sock . toLazyByteString)
          (  serialize revision (mkDataPacket "" (columnsCount @columns @record) (fromIntegral $ length records))
          <> serializeRecords @columns revision records
          )
      (sendAll sock . toLazyByteString)
        (serialize revision (mkDataPacket "" 0 0))
      handleInsertResult @columns @record conn buffer2 []
    EndOfStream         -> pure ()
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (ProtocolImplementationError $ UnexpectedPacketType otherPacket)




-- * Bufferization

type Buffer = LazyByteString

emptyBuffer :: Buffer
emptyBuffer = BL.Empty

rawBufferizedRead :: Buffer -> Get packet -> Socket -> Int64 -> IO (packet, Buffer)
rawBufferizedRead buffer parser sock bufSize = runBufferReader (recv sock bufSize) buffer (runGetIncremental parser)

runBufferReader :: IO LazyByteString -> Buffer -> Decoder packet -> IO (packet, Buffer)
runBufferReader bufferFiller buffer = \case
  (Partial decoder) -> case buffer of
    BL.Chunk bs mChunk -> runBufferReader bufferFiller mChunk (decoder $ Just bs)
    BL.Empty ->
      bufferFiller >>= \case
        BL.Chunk bs mChunk -> runBufferReader bufferFiller mChunk (decoder $ Just bs)
        BL.Empty -> throwIO (DeserializationError "Expected more bytes while reading packet")
  (Done !leftover _consumed !packet) -> pure (packet, fromStrict leftover)
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
