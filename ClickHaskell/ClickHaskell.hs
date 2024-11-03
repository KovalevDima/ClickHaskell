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
import ClickHaskell.NativeProtocol.ClientPackets (HelloParameters (..), mkAddendum, mkDataPacket, mkHelloPacket, mkPingPacket, mkQueryPacket)
import ClickHaskell.NativeProtocol.Columns (Column (..), Columns (..), emptyColumns, KnownColumns (..), HasColumns(..), KnownColumn (..), appendColumn, mkColumn)
import ClickHaskell.NativeProtocol.Serialization (Deserializable (..), ProtocolRevision, Serializable (..), latestSupportedRevision)
import ClickHaskell.NativeProtocol.ServerPackets (ExceptionPacket, HelloResponse (..), ServerPacketType (..))

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.Binary.Get (Decoder (..), runGetIncremental)
import Data.ByteString.Char8 as BS8 (pack)
import Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import Data.ByteString.Lazy.Char8 as BSL8 (head)
import Data.ByteString.Lazy.Internal as BL (ByteString (..), LazyByteString)
import Data.Char (ord)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Typeable (Proxy(..))
import Data.Word (Word32)
import GHC.Generics
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
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

instance HasColumns (Table name columns)
  where
  type GetColumns (Table _ columns) = columns


newtype Table (name :: Symbol) (columns :: [Type]) = MkTable (Columns columns)
instance KnownColumns (Columns columns) => KnownColumns (Table name columns) where
  type ColumnsCount (Table name columns) = ColumnsCount (Columns columns)
  columnsCount = columnsCount @(Columns columns)
  rowsCount (MkTable columns) = rowsCount columns

data View (name :: Symbol) (columns :: [Type]) parameters

selectFrom ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , KnownSymbol name
  , ReadableFrom table record
  )
  =>
  Connection -> IO [record]
selectFrom MkConnection{sock, user, chosenRevision} = do
  let query
        =  "SELECT " <> readingColumns @table @record
        <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkQueryPacket chosenRevision user (toChType query))
    <> serialize chosenRevision (mkDataPacket "" emptyColumns)
    )
  _ <- recv sock 4096
  pure []


select ::
  forall columns record
  .
  ReadableFrom (Columns columns) record
  =>
  Connection -> ChString -> IO [record]
select MkConnection{sock, user, chosenRevision} query = do
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkQueryPacket chosenRevision user query)
    <> serialize chosenRevision (mkDataPacket "" emptyColumns)
    )
  _ <- recv sock 4096
  pure []


insertInto ::
  forall table record name columns
  .
  ( table ~ Table name columns
  , WritableInto (Table name columns) record
  , KnownSymbol name
  , Serializable (Columns columns)
  )
  => Connection -> [record] -> IO ()
insertInto MkConnection{sock, user, chosenRevision} columns = do
  let query =
        "INSERT INTO " <> (byteString . BS8.pack) (symbolVal $ Proxy @name)
        <> " (" <> writingColumns @table @record <> ") VALUES"
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkQueryPacket chosenRevision user (toChType query))
    <> serialize chosenRevision (mkDataPacket "" emptyColumns)
    )
  print =<< recv sock 4096
  -- ^ answers with 11.TableColumns
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkDataPacket "" . toColumns @(Table name columns) $ columns)
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

-- ** Reading

type GenericReadable record hasColumns =
  ( Generic record
  , GReadable (GetColumns hasColumns) (Rep record)
  )

class
  ( KnownColumns hasColumns
  , HasColumns hasColumns
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
    (:*:)
      <$> map (M1 . K1 . fromChType @chType) column
      <*> gFromColumns @restColumns @right extraColumns
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
  gToColumns =  gToColumns @columns . map (unM1 . unM1)
  gWritingColumns = gWritingColumns @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  gToColumns  = gToColumns . map (\((l1 :*: l2) :*: r) -> l1 :*: (l2 :*: r))
  gWritingColumns = gWritingColumns @columns @((left1 :*: left2) :*: right)

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
  _a <- selectFrom @ExampleTable @ExampleData connection
  print "Dummy queries done"
  insertInto @ExampleTable @ExampleData connection devColumns

devColumns ::  [ExampleData]
devColumns = MkExample <$> replicate 5 127


type ExampleTable = Table "example" '[Column "val" ChUInt32]

data ExampleData = MkExample
  { val :: Word32
  } deriving (Generic)

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
