{-# LANGUAGE 
    DeriveAnyClass
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
#-}

module ClickHaskell.Native
  ( ChCredential(..)
  , openNativeConnection
  , ping
  , selectFrom
  ) where

-- Internal dependencies
import ClickHaskell.Native.Packets
  ( ChCredential(..)
  , User
  , mkHelloPacket
  , mkPingPacket
  , mkQueryPacket
  , mkDataPacket
  , ServerPacketType (..)
  , determineServerPacket
  )
import ClickHaskell.Native.Versioning (latestSupportedRevision)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.ByteString.Builder (toLazyByteString)
import Data.Maybe (fromMaybe, listToMaybe)
import Network.Socket
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString.Lazy (recv, sendAll)

data ClientError
  = ConnectionError ConnectionError
  | ProtocolImplementationError ProtocolImplementationError
  deriving (Show, Exception)

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)

{- |
  You shouldn't see this exceptions. Please report a bug if it appears
-}
data ProtocolImplementationError
  = UnexpectedPacketType ServerPacketType
  | UnknownPacketType
  deriving (Show, Exception)


data Connection = MkConnection
  { sock :: Socket
  , user :: User
  }

openNativeConnection :: ChCredential -> IO Connection
openNativeConnection credentials@MkChCredential{chHost, chPort, chLogin} = do
  AddrInfo
      { addrFamily
      , addrSocketType
      , addrProtocol
      , addrAddress
      } <- fromMaybe (throw $ ConnectionError NoAdressResolved) . listToMaybe
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
         setSocketOption sock KeepAlive 1
         connect sock addrAddress
         pure sock
      )

  (sendAll sock . toLazyByteString) (mkHelloPacket latestSupportedRevision credentials)
  serverPacketType <- determineServerPacket sock
  print serverPacketType
  print =<< recv sock 4096
  pure $ MkConnection{sock, user=chLogin} 


ping :: Connection -> IO ()
ping MkConnection{sock} = do
  (sendAll sock . toLazyByteString) mkPingPacket
  responscePacket <- determineServerPacket sock
  case responscePacket of
    Just Pong -> pure ()
    Just Exception -> pure ()
    Just otherPacket -> throw . ProtocolImplementationError $ UnexpectedPacketType otherPacket
    Nothing -> throw . ProtocolImplementationError $ UnknownPacketType


selectFrom :: Connection -> IO ()
selectFrom MkConnection{sock, user} = do
  (sendAll sock . toLazyByteString)
    (  mkQueryPacket latestSupportedRevision user "SELECT 5"
    <> mkDataPacket "" False
    )
  _ <- recv sock 4096
  pure ()
