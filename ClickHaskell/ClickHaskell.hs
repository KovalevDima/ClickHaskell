{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DuplicateRecordFields
  , LambdaCase
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , UndecidableInstances
  , RecordWildCards
#-}

module ClickHaskell
  ( module ClickHaskell.DbTypes
  , module ClickHaskell.Tables
  , ChCredential(..)
  , openNativeConnection
  , selectFrom
  , insertInto
  , ping
  , dev
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol.ClientPackets (mkPingPacket, mkQueryPacket, mkDataPacket, mkHelloPacket, HelloParameters(..))
import ClickHaskell.NativeProtocol.Serialization (Deserializable(..), Serializable(..), ProtocolRevision, latestSupportedRevision)
import ClickHaskell.NativeProtocol.ServerPackets (ServerPacketType(..), HelloResponse(..))
import ClickHaskell.Tables

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.Binary.Get (Decoder (..), runGetIncremental)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy.Char8 as BSL8 (head)
import Data.ByteString.Lazy.Internal as BL (ByteString (..), LazyByteString)
import Data.Char (ord)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
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
  { sock :: Socket
  , user :: ChString
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
      MkHelloResponse{server_revision} <- bufferizedRead latestSupportedRevision (recv sock 4096)
      pure MkConnection{user=toChType chLogin, sock, chosenRevision=min server_revision latestSupportedRevision}
    Exception -> do
      print =<< recv sock 4096
      throw DatabaseException
    otherPacket -> throw . ProtocolImplementationError $ UnexpectedPacketType otherPacket




-- * Ping

ping :: Connection -> IO ()
ping MkConnection{sock, chosenRevision} = do
  (sendAll sock . toLazyByteString) (mkPingPacket chosenRevision)
  responscePacket <- determineServerPacket sock
  case responscePacket of
    Pong -> pure ()
    Exception -> throw DatabaseException
    otherPacket -> throw . ProtocolImplementationError $ UnexpectedPacketType otherPacket


-- * Querying
selectFrom :: Connection -> IO ()
selectFrom MkConnection{sock, user, chosenRevision} = do
  (sendAll sock . toLazyByteString)
    (  serialize chosenRevision (mkQueryPacket chosenRevision user "SELECT 5")
    <> serialize chosenRevision (mkDataPacket "" emptyColumns)
    )
  _ <- recv sock 4096
  pure ()


insertInto :: Connection -> Columns columns -> IO ()
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
  -- ^ asnwers with 1.Data

-- * 

determineServerPacket :: Socket -> IO ServerPacketType
determineServerPacket sock = do
  headByte <- ord . BSL8.head <$> recv sock 1
  pure $
    if headByte <= fromEnum (maxBound :: ServerPacketType)
    then toEnum headByte
    else throw $ ProtocolImplementationError UnknownPacketType


-- ** Bufferized reading

bufferizedRead :: forall packet . Deserializable packet => ProtocolRevision -> IO LazyByteString -> IO packet
bufferizedRead rev bufferFiller = runBufferReader bufferFiller (runGetIncremental (deserialize @packet rev)) BL.Empty

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
  | DatabaseException
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




-- * Dev

dev :: IO ()
dev = do
  connection <- openNativeConnection devCredential
  print "Connected"
  -- replicateM_ 500 (ping connection)
  -- print "Pinged"
  -- replicateM_ 500 (selectFrom connection)
  -- print "Dummy queries done"
  insertInto connection devColumns

devColumns :: Columns '[Column "val" ChUInt32]
devColumns = appendColumn devColumn emptyColumns

devColumn :: Column "val" ChUInt32
devColumn = mkColumn [5, 5, 5]

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
