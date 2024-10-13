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
import Data.ByteString (toStrict)
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString (recv, send)


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
  putStrLn "🎬"

  putStrLn "Hello packet sending💬"
  _ <- (send sock . toStrict . toLazyByteString)
    (mkHelloPacket devCredential)

  putStrLn "Hello packet reading👂"
  print =<< recv sock 4096


  putStrLn "Ping packet sending💬"
  _ <- (send sock . toStrict . toLazyByteString)
    mkPingPacket

  putStrLn "Ping packet reading👂"
  print =<< recv sock 4096


  putStrLn "Query packet sending💬"
  _ <- (send sock . toStrict . toLazyByteString)
    (  mkQueryPacket 54_429 devCredential "SELECT 'hello, world!';"
    <> mkDataPacket 54_429 "" False
    )

  putStrLn "Query packet reading👂"
  print =<< recv sock 4096

  putStrLn "🎬"

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = "default"
  , chHost = "localhost"
  , chPort = "9000"
  }
