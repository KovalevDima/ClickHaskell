module ClickHaskell.Connection where

-- Internal
import ClickHaskell.Primitive

-- GHC included
import Control.Concurrent (MVar)
import Control.Exception (throwIO, SomeException, finally, catch, bracketOnError)
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.Binary.Get
import Data.ByteString as BS (ByteString, length)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import GHC.Exception (Exception)
import Prelude hiding (liftA2)

-- External
import Network.Socket hiding (SocketOption(..))
import Network.Socket (SocketOption(..))
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)
import System.Timeout (timeout)



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

writeToConnection :: ConnectionState -> (ProtocolRevision -> Builder) -> IO ()
writeToConnection MkConnectionState{revision, buffer} serializer =
  (writeSock buffer) (serializer revision)

data Connection where MkConnection :: (MVar ConnectionState) -> Connection

data ConnectionState = MkConnectionState
  { buffer       :: Buffer
  , revision     :: ProtocolRevision
  , initial_user :: ChString
  , os_user      :: ChString
  , hostname     :: ChString
  , creds        :: ConnectionArgs
  }

createConnectionState
  :: (Buffer -> ConnectionArgs -> IO ConnectionState)
  -> ConnectionArgs
  -> IO ConnectionState
createConnectionState postInitAction creds@MkConnectionArgs {initBuffer, host,initSocket, resolveAddrName} = do
  buffer <- initBuffer host =<< initSocket =<< resolveAddrName creds
  postInitAction buffer creds

recreateConnectionState
  :: (Buffer -> ConnectionArgs -> IO ConnectionState)
  -> ConnectionState
  -> IO ConnectionState
recreateConnectionState postInitAction MkConnectionState{creds, buffer} = do
  flushBuffer buffer
  closeSock buffer
  createConnectionState postInitAction creds

data Buffer = MkBuffer
  { readSock :: IO BS.ByteString
  , writeSock :: Builder -> IO ()
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
  { user :: String
  , pass :: String
  , db   :: String
  , host :: HostName
  , mPort :: Maybe ServiceName
  , defPort :: ServiceName
  , mOsUser :: Maybe String
  , mHostname :: Maybe String
  , resolveAddrName :: ConnectionArgs -> IO AddrInfo
  , initSocket :: AddrInfo -> IO Socket
  , initBuffer :: HostName -> Socket -> IO Buffer
  }

{- |
  Default connection settings which follows __clickhouse-client__ defaults

  Use `setUser`, `setPassword`, `setHost`, `setPort`, `setDatabase`
  to modify connection defaults.
-}
defaultConnectionArgs :: ConnectionArgs
defaultConnectionArgs = MkConnectionArgs
  { user = "default"
  , pass = ""
  , host = "localhost"
  , db   = "default"
  , defPort = "9000"
  , mPort = Nothing
  , mOsUser = Nothing
  , mHostname = Nothing
  , initSocket = \AddrInfo{..} -> do
      maybe (throwIO EstablishTimeout) pure
        =<< timeout 3_000_000 (
          bracketOnError
            (socket addrFamily addrSocketType addrProtocol)
            (\sock ->
              catch @SomeException
                (finally (shutdown sock ShutdownBoth) (close sock))
                (const $ pure ())
            )
          (\sock -> do
            setSocketOption sock NoDelay 1
            setSocketOption sock KeepAlive 1
            connect sock addrAddress
            pure sock
          )
        )
  , resolveAddrName = \MkConnectionArgs {host, mPort, defPort} -> do
      let hints = defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream}
          port  = fromMaybe defPort mPort
      addrs <- getAddrInfo (Just hints) (Just host) (Just port)
      case addrs of
        []  -> throwIO NoAdressResolved
        x:_ -> pure x
  , initBuffer = \_hostname sock -> do
      buff <- newIORef ""
      pure
        MkBuffer
          { writeSock = \bs -> (sendAll sock . toLazyByteString) bs
          , readSock  = recv sock 4096
          , closeSock = close sock
          , buff
          }
  }


{- |
  Overrides default user __"default"__
-}
setUser :: String -> ConnectionArgs -> ConnectionArgs
setUser new MkConnectionArgs{..} = MkConnectionArgs{user=new, ..}

{- |
  Overrides default password __""__
-}
setPassword :: String -> ConnectionArgs -> ConnectionArgs
setPassword new MkConnectionArgs{..} = MkConnectionArgs{pass=new, ..}

{- |
  Overrides default hostname __"localhost"__
-}
setHost :: HostName -> ConnectionArgs -> ConnectionArgs
setHost new MkConnectionArgs{..} = MkConnectionArgs{host=new, ..}

{- |
  Set a custom port instead of the default __9000__ (or __9443__ if TLS is used).

  The default port can only be overridden by 'overrideNetwork'.
-}
setPort :: ServiceName -> ConnectionArgs -> ConnectionArgs
setPort new MkConnectionArgs{..} = MkConnectionArgs{mPort=Just new, ..} 

{- |
  Overrides default database __"default"__
-}
setDatabase :: String -> ConnectionArgs -> ConnectionArgs
setDatabase new MkConnectionArgs{..} = MkConnectionArgs{db=new, ..}

{- |
  Overrides default hostname value which is:
  1. __$HOSTNAME__ variable value (if set)
  2. __""__ otherwise
-}
overrideHostname :: String -> ConnectionArgs -> ConnectionArgs
overrideHostname new MkConnectionArgs{..} = MkConnectionArgs{mHostname=Just new, ..}

{- |
  Overrides default os_name value which is:
  1. __$USER__ variable value (if set)
  2. __""__ otherwise
-}
overrideOsUser :: String -> ConnectionArgs -> ConnectionArgs
overrideOsUser new MkConnectionArgs{..} = MkConnectionArgs{mOsUser=Just new, ..}

overrideNetwork
  :: ServiceName
  -> (HostName -> Socket -> IO Buffer)
  -> (ConnectionArgs -> ConnectionArgs)
overrideNetwork
  newDefPort
  newInitBuffer
  MkConnectionArgs {..}
  =
  MkConnectionArgs
    { defPort = newDefPort
    , initBuffer = newInitBuffer
    , ..
    }
