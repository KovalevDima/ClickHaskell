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
  , ChCredential(..), ServerHelloResponse (..), readHelloPacket
  )
import ClickHaskell.Tables ()

-- GHC included
import Control.Exception (SomeException, bracketOnError, catch, finally, throw)
import Control.Monad (replicateM_)
import Data.ByteString.Builder (toLazyByteString)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Timeout (timeout)

-- External
import Network.Socket as Sock
import Network.Socket.ByteString.Lazy (recv, sendAll)


-- * Connection

data Connection = MkConnection
  { sock :: Socket
  , user :: ChString
  , chosenRevision :: ProtocolRevision 
  }

openNativeConnection :: ChCredential -> IO Connection
openNativeConnection cred@MkChCredential{chHost, chPort, chLogin} = do
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

  (sendAll sock . toLazyByteString . serialize latestSupportedRevision) (mkHelloPacket cred)
  
  serverPacketType <- determineServerPacket sock
  print serverPacketType

  case serverPacketType of
    HelloResponse -> do
      readedBytes <- recv sock 4096 
      print readedBytes
      let helloPacket = readHelloPacket readedBytes
          MkServerHelloResponse{server_revision} = helloPacket
      print helloPacket
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
    (  mkQueryPacket chosenRevision (fromChType user) "SELECT 5"
    <> mkDataPacket chosenRevision "" False
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

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
