{-# LANGUAGE
    DeriveGeneric
  , DuplicateRecordFields
  , OverloadedStrings
  , NumericUnderscores
  , NamedFieldPuns
  , RecordWildCards
#-}

module ClickHaskell
  ( module ClickHaskell.DbTypes
  , ChCredential(..)
  , openNativeConnection
  , ping
  , dev
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol
  ( ProtocolRevision
  , ServerPacketType(..)
  , latestSupportedRevision
  , Serializable(..)
  , determineServerPacket
  , ProtocolImplementationError(..)
  , ClientError(..)
  , ConnectionError(..)
  , mkPingPacket
  , mkQueryPacket
  , mkDataPacket
  , mkHelloPacket
  , ServerHelloResponse (..)
  , readHelloPacket, HelloParameters (..)
  )
import ClickHaskell.Tables ()

-- GHC included
import Control.Exception (SomeException, bracketOnError, catch, finally, throw)
import Control.Monad (replicateM_)
import Data.ByteString.Builder (toLazyByteString)
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
      MkServerHelloResponse{server_revision} <- readHelloPacket (recv sock 4096)
      pure MkConnection{user=toChType chLogin, sock, chosenRevision=server_revision}
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
    <> serialize chosenRevision (mkDataPacket "")
    )
  _ <- recv sock 4096
  pure ()


-- * Dev

dev :: IO ()
dev = do
  connection <- openNativeConnection devCredential
  print "Connected"
  replicateM_ 500 (ping connection)
  print "Pinged"
  replicateM_ 500 (selectFrom connection)
  print "Dummy queries done"

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
