{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, NamedFieldPuns, NumericUnderscores #-} 
module ClickHaskell.Native where

-- Internal dependencies
import ClickHaskell.DbTypes

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import Data.ByteString.Char8 as BS8 (toStrict)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text as Text (Text, unpack)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Bits (..), Ptr)
import GHC.Generics (Generic)
import Network.Socket
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString (recv, send)
import Data.Text.Encoding (encodeUtf8Builder)

openNativeConnection :: ChCredential -> IO (Socket, SockAddr)
openNativeConnection MkChCredential{chHost} = do
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
      (Just $ Text.unpack chHost)
      (Just "tcp")

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
  , chHost = "localhost:9000"
  }


data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: Text
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

instance Serializable ChUInt64 where
  serialize = vlq128 . fromChType @ChUInt64 @Word64

dev :: IO ()
dev = do
  (sock, _sockAddr) <- openNativeConnection devCredential
  sendHello devCredential sock


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


sendHello :: ChCredential -> Socket -> IO ()
sendHello MkChCredential{chDatabase, chLogin, chPass} sock = do
  _sentSize <- send sock
    (toStrict . toLazyByteString . mconcat $
      [ vlq128 @Word8 0            -- Hello packet code
      , vlq128 @Word8 5, "hello"   -- Client name: "Hello"
      , vlq128 @Word8 0            -- Major version: 0
      , vlq128 @Word8 1            -- Minor version: 0
      , vlq128 @Word16 55_255      -- Protocol version
      , vlq128 @Word8 7, encodeUtf8Builder chDatabase -- Database name: "default"
      , vlq128 @Word8 7, encodeUtf8Builder chLogin -- User name: "default"
      , vlq128 @Word8 0, encodeUtf8Builder chPass        -- Password: ""
      ]
    )
  bs <- recv sock 4096
  print bs
