{-# LANGUAGE 
    OverloadedStrings
  , DeriveAnyClass
  , NamedFieldPuns
  , NumericUnderscores
#-}
module ClickHaskell.Native where

-- Internal dependencies
import ClickHaskell.Native.Packets (mkHelloPacket, mkQueryPacket, ChCredential(..), mkPingPacket, mkDataPacket)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.Maybe (fromMaybe, listToMaybe)
import Network.Socket
import Data.ByteString.Builder (toLazyByteString)
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString.Lazy (recv, sendAll)


data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)

openNativeConnection :: ChCredential -> IO (Socket, SockAddr)
openNativeConnection MkChCredential{chHost, chPort} = do
  AddrInfo
    { addrFamily
    , addrSocketType
    , addrProtocol
    , addrAddress
    } <-
      fromMaybe (throw NoAdressResolved)
    . listToMaybe
    <$>
    getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just chHost)
      (Just chPort)

  (fromMaybe (throw EstablishTimeout) <$>) . timeout 3_000_000 $
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
         pure (sock, addrAddress)
      )

dev :: IO ()
dev = do
  (sock, _sockAddr) <- openNativeConnection devCredential

  (sendAll sock . toLazyByteString) (mkHelloPacket 54467 devCredential)
  (sendAll sock . toLazyByteString) "\0"
  print =<< recv sock 4096

  (sendAll sock . toLazyByteString) mkPingPacket
  print =<< recv sock 4096

  (sendAll sock . toLazyByteString)
    (  mkQueryPacket 54467 devCredential "CREATE VIEW hello AS SELECT 5"
    <> mkDataPacket "" False
    )
  print =<< recv sock 4096

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = ""
  , chHost = "localhost"
  , chPort = "9000"
  }
