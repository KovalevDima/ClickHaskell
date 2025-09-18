module ClickHaskell.Connection where

-- Internal
import ClickHaskell.Primitive

-- GHC included
import Control.Concurrent (MVar)
import Control.Exception (throwIO, SomeException, finally, catch, bracketOnError)
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString as BS (ByteString, length)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
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
  (writeConn buffer) (serializer revision)

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
createConnectionState postInitAction creds@MkConnectionArgs{..} = do
  let port = fromMaybe defPort mPort
  buffer <- initBuffer host =<< initSocket =<< resolveHostName host port
  postInitAction buffer creds

recreateConnectionState
  :: (Buffer -> ConnectionArgs -> IO ConnectionState)
  -> ConnectionState
  -> IO ConnectionState
recreateConnectionState postInitAction MkConnectionState{creds, buffer} = do
  destroyBuff buffer
  createConnectionState postInitAction creds

data Buffer = MkBuffer
  { readBuff :: IO BS.ByteString
  , destroyBuff :: IO ()
  , writeBuff :: BS.ByteString -> IO ()
  , writeConn :: Builder -> IO ()
  }




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
  , resolveHostName :: HostName -> ServiceName -> IO AddrInfo
  , initBuffer :: HostName -> Socket -> IO Buffer
  , initSocket :: AddrInfo -> IO Socket
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
  , resolveHostName = defaultResolveHostName
  , initSocket = defaultInitSocket
  , initBuffer = defaultInitBuffer
  }

defaultResolveHostName :: HostName -> ServiceName -> IO AddrInfo
defaultResolveHostName host port = do
  let hints = defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    []  -> throwIO NoAdressResolved
    x:_ -> pure x

defaultInitSocket :: AddrInfo -> IO Socket
defaultInitSocket AddrInfo{..} = do
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

defaultInitBuffer :: HostName -> Socket -> IO Buffer
defaultInitBuffer _hostname sock = 
  mkBuffer
    (sendAll sock . toLazyByteString)
    (recv sock 4096)
    (catch @SomeException
      (finally (shutdown sock ShutdownBoth) (close sock))
      (const $ pure ())
    )

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




-- * Overriders

{- |
  This function should be used when you want to override
  the default connection behaviour

  Designed to be passed into 'overrideNetwork'

  Watch __ClickHaskell-tls__ package for example
-}
mkBuffer
  :: (Builder -> IO ())
  -> IO ByteString
  -> IO ()
  -> IO Buffer 
mkBuffer sendSock readSock closeSock = do
  buff <- newIORef ""
  let writeBuff bs = atomicWriteIORef buff bs

  pure MkBuffer
    { writeConn = sendSock
    , writeBuff
    , readBuff = do
      currentBuffer <- readIORef buff
      case BS.length currentBuffer of
        0 -> readSock
        _ -> writeBuff "" *> pure currentBuffer
    , destroyBuff = do
      closeSock
      writeBuff ""
    }

{- |
  Overrides default client hostname value which is:

  1. __$HOSTNAME__ env variable value (if set)
  2. __""__ otherwise

  Client hostname being displayed in ClickHouse logs
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

overrideInitConnection :: (HostName -> Socket -> IO Buffer) -> (ConnectionArgs -> ConnectionArgs)
overrideInitConnection new MkConnectionArgs {..} =
  MkConnectionArgs{initBuffer = new, ..}

{- |
Override the default port used when no port was set explicitly via 'setPort'.

This does not immediately fix the connection port:
if the user has already called 'setPort', that value takes precedence.
Otherwise, the given port becomes the new default.

Typical use case: provide a different default for TLS connections, e.g. 9443.
-}
overrideDefaultPort :: ServiceName -> (ConnectionArgs -> ConnectionArgs)
overrideDefaultPort new MkConnectionArgs {..} =
  MkConnectionArgs {defPort = new, ..}
