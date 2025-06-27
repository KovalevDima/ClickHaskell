module ClickHaskell.Connection where

-- Internal
import ClickHaskell.Primitive

-- GHC included
import Control.Concurrent (MVar)
import Control.Exception (throwIO)
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.Binary.Get
import Data.ByteString as BS (ByteString, length)
import Data.ByteString.Lazy as BSL (ByteString)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import Data.Text (Text)
import GHC.Exception (Exception)
import Prelude hiding (liftA2)

-- External
import Network.Socket hiding (SocketOption(..))
import Network.Socket qualified as Sock (SocketOption(..))
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)



-- * Connection

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

writeToConnection :: Serializable packet => ConnectionState -> packet -> IO ()
writeToConnection MkConnectionState{revision, buffer} packet =
  (writeSock buffer . toLazyByteString . serialize revision) packet

writeToConnectionEncode :: ConnectionState -> (ProtocolRevision -> Builder) -> IO ()
writeToConnectionEncode MkConnectionState{revision, buffer} serializer =
  (writeSock buffer . toLazyByteString) (serializer revision)

data Connection where MkConnection :: (MVar ConnectionState) -> Connection

data ConnectionState = MkConnectionState
  { user     :: ChString
  , hostname :: ChString
  , os_user  :: ChString
  , buffer   :: Buffer
  , revision :: ProtocolRevision
  , creds    :: ConnectionArgs
  }

data Buffer = MkBuffer
  { readSock :: IO BS.ByteString
  , writeSock :: BSL.ByteString -> IO ()
  , closeSock :: IO ()
  , buff :: IORef BS.ByteString
  }

flushBuffer :: Buffer -> IO ()
flushBuffer MkBuffer{buff} = atomicWriteIORef buff ""

rawBufferRead :: Buffer -> Get packet -> IO packet
rawBufferRead buffer@MkBuffer{..} parser = runBufferReader (runGetIncremental parser)
  where
  runBufferReader :: Decoder packet -> IO packet
  runBufferReader = \case
    (Partial decoder) -> readBuffer >>= runBufferReader . decoder . Just
    (Done leftover _consumed packet) -> packet <$ atomicModifyIORef buff (leftover,)
    (Fail _leftover _consumed msg) -> throwIO  (DeserializationError msg)

  readBuffer :: IO BS.ByteString
  readBuffer =
    readIORef buff
      >>= (\currentBuffer ->
        case BS.length currentBuffer of
          0 -> readSock
          _ -> flushBuffer buffer *> pure currentBuffer
      )




-- * Initialization

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
