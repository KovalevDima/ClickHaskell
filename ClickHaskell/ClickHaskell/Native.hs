{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, NamedFieldPuns, NumericUnderscores #-}
module ClickHaskell.Native where

-- Internal dependencies
import ClickHaskell.DbTypes
import Paths_ClickHaskell (version)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.ByteString as BS (length)
import Data.ByteString.Builder as BS (Builder, byteString, toLazyByteString, word8)
import Data.ByteString.Char8 as BS8 (ByteString, toStrict)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text as Text (Text)
import Data.Version (Version (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Bits (..), Ptr)
import GHC.Generics (Generic)
import Network.Socket
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString (recv, send)

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

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = "default"
  , chHost = "localhost"
  , chPort = "9000"
  }


data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: HostName
  , chPort     :: ServiceName
  }
  deriving (Generic, Show, Eq)

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)

class
  Deserializable chType
  where
  deserialize :: Ptr Word8 -> chType

class Serializable chType
  where
  serialize :: chType -> Builder

instance Serializable ChUInt8 where
  serialize = vlq128 . fromChType @ChUInt8 @Word8

instance Serializable ChUInt16 where
  serialize = vlq128 . fromChType @ChUInt16 @Word16

instance Serializable ChUInt32 where
  serialize = vlq128 . fromChType @ChUInt32 @Word32

instance Serializable ChUInt64 where
  serialize = vlq128 . fromChType @ChUInt64 @Word64

instance Serializable ChUInt128 where
  serialize = vlq128 . fromChType @ChUInt128 @Word128

instance Serializable ChString where
  serialize str
    =  (serialize @ChUInt64 . fromIntegral . BS.length . fromChType @_ @ByteString) str
    <> (BS.byteString . fromChType @_ @ByteString) str

dev :: IO ()
dev = do
  (sock, _sockAddr) <- openNativeConnection devCredential
  sendHelloPacket devCredential sock
  sendQueryPacket sock "SELECT 5"


{-# SPECIALIZE vlq128 :: Word8 -> Builder #-}
{-# SPECIALIZE vlq128 :: Word16 -> Builder #-}
{-# SPECIALIZE vlq128 :: Word32 -> Builder #-}
{-# SPECIALIZE vlq128 :: Word64 -> Builder #-}
vlq128 :: (Bits a, Num a, Integral a) => a -> Builder
vlq128 = go
  where
  go i
    | i <= 127  = word8 (fromIntegral i :: Word8)
    | otherwise = word8 (0x80 .&. (fromIntegral i .|. 0x7F)) <> go (i `unsafeShiftR` 7)



sendHelloPacket :: ChCredential -> Socket -> IO ()
sendHelloPacket MkChCredential{chDatabase, chLogin, chPass} sock = do
  _sentSize <- send sock
    (toStrict . toLazyByteString . mconcat $
      -- Hello packet code
      [ serialize @ChUInt8 0
      -- Client name
      , serialize @ChString "ClickHaskell"
      -- Major version
      , serialize @ChUInt16 (fromIntegral $ versionBranch version !! 0)
      -- Minor version
      , serialize @ChUInt16 (fromIntegral $ versionBranch version !! 1)
      -- Protocol version
      , serialize @ChUInt16 55_255
      -- Database name
      , serialize @ChString (toChType chDatabase)
      -- User name
      , serialize @ChString (toChType chLogin)
      -- Password
      , serialize @ChString (toChType chPass)
      ]
    )
  bs <- recv sock 4096
  print bs

sendQueryPacket :: Socket -> ChString -> IO ()
sendQueryPacket sock query = do
  _sentSize <- send sock
    (toStrict . toLazyByteString . mconcat $
      [ serialize @ChString "someId", serialize @ChString ""
      , serialize @ChString ""
      , serialize @ChString ""
      , serialize @ChUInt8 2
      , serialize @ChUInt8 0
      , serialize @ChString query
      , serialize @ChString ""
      ]
    )
  bs <- recv sock 4096
  print bs
