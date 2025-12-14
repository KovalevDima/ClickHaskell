module ClickHaskell.Connection where

-- Internal
import ClickHaskell.Primitive

-- GHC included
import Control.Concurrent (MVar)
import Control.Exception (SomeException, bracketOnError, catch, finally, throwIO)
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString as BS (ByteString, null)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import GHC.Exception (Exception)

-- External
import Network.Socket
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
  | ServerClosedConnection
  -- ^ Occurs on 'readSock' empty @""@ result
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
  buffer <- mkBuffer =<< mkBufferArgs host =<< initSocket =<< resolveHostName host port
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

mkBuffer :: BufferArgs -> IO Buffer
mkBuffer MkBufferArgs{..} = do
  buff <- newIORef ""
  let writeBuff bs = atomicWriteIORef buff bs

  pure MkBuffer
    { writeConn = writeSock
    , writeBuff
    , readBuff = do
      currentBuffer <- readIORef buff
      if (not . BS.null) currentBuffer 
      then writeBuff "" *> pure currentBuffer
      else do
        sockBytes <- readSock
        if BS.null sockBytes
        then throwIO ServerClosedConnection
        else pure sockBytes
    , destroyBuff = do
      closeSock
      writeBuff ""
    }

data BufferArgs = MkBufferArgs
  { writeSock :: Builder -> IO ()
  , readSock  :: IO ByteString
  , closeSock :: IO ()
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
  , maxRevision :: ProtocolRevision
  , mOsUser :: Maybe String
  , mHostname :: Maybe String
  , resolveHostName :: HostName -> ServiceName -> IO AddrInfo
  , mkBufferArgs :: HostName -> Socket -> IO BufferArgs
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
  , maxRevision = mkRev @DBMS_TCP_PROTOCOL_VERSION
  , resolveHostName = defaultResolveHostName
  , initSocket = defaultInitSocket
  , mkBufferArgs = defaultMkBufferArgs
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

defaultMkBufferArgs :: HostName -> Socket -> IO BufferArgs
defaultMkBufferArgs _hostname sock =
  pure $
    let
    writeSock = sendAll sock . toLazyByteString
    readSock = recv sock 4096
    closeSock =
      catch @SomeException
        (finally (shutdown sock ShutdownBoth) (close sock))
        (const $ pure ())
    in MkBufferArgs{..}

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

{- |
  This function should be used when you want to override
  the default connection behaviour

  Watch __ClickHaskell-tls__ package for example
-}
overrideInitConnection :: (HostName -> Socket -> IO BufferArgs) -> (ConnectionArgs -> ConnectionArgs)
overrideInitConnection new MkConnectionArgs {..} =
  MkConnectionArgs{mkBufferArgs = new, ..}

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

overrideMaxRevision :: ProtocolRevision -> (ConnectionArgs -> ConnectionArgs)
overrideMaxRevision new MkConnectionArgs {..} =
  MkConnectionArgs {maxRevision = new, ..}
