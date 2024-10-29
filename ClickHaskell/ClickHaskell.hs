{-# LANGUAGE
    AllowAmbiguousTypes
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

module ClickHaskell
  ( module ClickHaskell.DbTypes
  , ChCredential(..)
  , openNativeConnection
  , Table
  , View
  , selectFrom
  , insertInto
  , ping
  , dev
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol.ClientPackets (HelloParameters (..), mkAddendum, mkDataPacket, mkHelloPacket, mkPingPacket, mkQueryPacket)
import ClickHaskell.NativeProtocol.Columns (Column (..), Columns, appendColumn, emptyColumns)
import ClickHaskell.NativeProtocol.Serialization (Deserializable (..), ProtocolRevision, Serializable (..), latestSupportedRevision)
import ClickHaskell.NativeProtocol.ServerPackets (ExceptionPacket, HelloResponse (..), ServerPacketType (..))

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.Binary.Get (Decoder (..), runGetIncremental)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy.Char8 as BSL8 (head)
import Data.ByteString.Lazy.Internal as BL (ByteString (..), LazyByteString)
import Data.Char (ord)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics
import GHC.TypeLits (Symbol)
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
  { sock           :: Socket
  , user           :: ChString
  , bufferSize     :: Int64
  , chosenRevision :: ProtocolRevision
  }

openNativeConnection :: ChCredential -> IO Connection
openNativeConnection MkChCredential{chHost, chPort, chLogin, chPass, chDatabase} = do
  AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress}
    <- fromMaybe (throw $ ConnectionError NoAdressResolved) . listToMaybe
    <$> getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just chHost)
      (Just chPort)
  sock <- (fromMaybe (throw $ ConnectionError EstablishTimeout) <$>) . timeout 3_000_000 $
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

  serverPacketType <- determineServerPacket sock
  case serverPacketType of
    HelloResponse -> do
      MkHelloResponse{server_revision} <- rawBufferizedRead latestSupportedRevision sock 4096
      (sendAll sock . toLazyByteString . serialize server_revision) mkAddendum
      pure MkConnection
        { user = toChType chLogin
        , chosenRevision = min server_revision latestSupportedRevision
        , sock
        , bufferSize = 4096
        }
    Exception ->
      throw . DatabaseException
        =<< rawBufferizedRead @ExceptionPacket latestSupportedRevision sock 4096
    otherPacket -> throw . ProtocolImplementationError $ UnexpectedPacketType otherPacket




-- * Ping

ping :: Connection -> IO ()
ping conn@MkConnection{sock, chosenRevision} = do
  (sendAll sock . toLazyByteString) (mkPingPacket chosenRevision)
  responscePacket <- determineServerPacket sock
  case responscePacket of
    Pong -> pure ()
    Exception -> throw . DatabaseException =<< readDeserializable conn
    otherPacket -> (throw . ProtocolImplementationError . UnexpectedPacketType) otherPacket




-- * Querying

data Table (name :: Symbol) columns = MkTable (Columns columns)
data View (name :: Symbol) (columns :: [Type]) parameters

selectFrom :: forall table record . (ReadableFrom table record) => Connection -> IO [record]
selectFrom MkConnection{sock, user, chosenRevision} = do
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkQueryPacket chosenRevision user "SELECT 5")
    <> serialize chosenRevision (mkDataPacket "" emptyColumns)
    )
  _ <- recv sock 4096
  pure []


insertInto :: Serializable (Columns columns) => Connection -> Columns columns -> IO ()
insertInto MkConnection{sock, user, chosenRevision} columns = do
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkQueryPacket chosenRevision user "INSERT INTO example (val) VALUES")
    <> serialize chosenRevision (mkDataPacket "" emptyColumns)
    )
  print =<< recv sock 4096
  -- ^ answers with 11.TableColumns
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkDataPacket "" columns)
    <> serialize chosenRevision (mkDataPacket "" emptyColumns)
    )
  print =<< recv sock 4096




-- * Reading

determineServerPacket :: Socket -> IO ServerPacketType
determineServerPacket sock = do
  headByte <- ord . BSL8.head <$> recv sock 1
  pure $
    if headByte <= fromEnum (maxBound :: ServerPacketType)
    then toEnum headByte
    else throw $ ProtocolImplementationError UnknownPacketType

readDeserializable :: forall packet . Deserializable packet => Connection -> IO packet
readDeserializable MkConnection{chosenRevision, sock, bufferSize} = rawBufferizedRead chosenRevision sock bufferSize

-- ** Bufferization

rawBufferizedRead :: forall packet . Deserializable packet => ProtocolRevision -> Socket -> Int64 -> IO packet
rawBufferizedRead rev sock bufferSize = runBufferReader (recv sock bufferSize) (runGetIncremental (deserialize @packet rev)) BL.Empty

runBufferReader :: Deserializable packet => IO LazyByteString -> Decoder packet -> LazyByteString -> IO packet
runBufferReader bufferFiller (Partial decoder) (BL.Chunk bs mChunk)
  = runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader bufferFiller (Partial decoder) BL.Empty = do
  bufferFiller >>= \case
    BL.Empty -> fail "Expected more bytes while reading packet" -- ToDo: Pass packet name
    BL.Chunk bs mChunk -> runBufferReader bufferFiller (decoder $ Just bs) mChunk
runBufferReader _bufferFiller (Done _leftover _consumed helloPacket) _input = pure helloPacket
runBufferReader _bufferFiller (Fail _leftover _consumed msg) _currentBuffer = error msg




-- * Errors handling

data ClientError
  = ConnectionError ConnectionError
  | DatabaseException ExceptionPacket
  | ProtocolImplementationError ProtocolImplementationError
  deriving (Show, Exception)

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

class
  ReadableFrom columns record
  where
  default fromColumns :: (Generic record, GReadable columns (Rep record)) => columns -> [record]
  fromColumns :: columns -> [record]
  fromColumns = map to . gFromColumns @columns

  default readingColumns :: (Generic record, GReadable columns (Rep record)) => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @columns @(Rep record)

instance
  ReadableFrom (Table name columns) record
  =>
  ReadableFrom (Columns columns) record
  where
  fromColumns columns = fromColumns @(Table name columns) (MkTable columns)
  readingColumns = readingColumns @(Table name columns) @record

class GReadable columns f
  where
  gFromColumns :: columns -> [f p]
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  gFromColumns = map (M1 . M1) . gFromColumns @columns
  gReadingColumns = gReadingColumns @columns @f




-- * Dev

dev :: IO ()
dev = do
  connection <- openNativeConnection devCredential
  print "Connected"
  ping connection
  print "Pinged"
  _a <- selectFrom @ExampleTable @ExampleData connection
  print "Dummy queries done"
  insertInto connection devColumns

devColumns :: Columns '[Column "val" ChUInt32]
devColumns = appendColumn devColumn emptyColumns

devColumn :: Column "val" ChUInt32
devColumn = MkColumn 500 $ replicate 500 5

type ExampleTable = Table "example" '[Column "val" ChUInt32]

data ExampleData = MkExample
  { val :: Word32
  }

instance ReadableFrom ExampleTable ExampleData where
  fromColumns _ = [MkExample{val=5}]
  readingColumns = "val"

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
