{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, NamedFieldPuns, NumericUnderscores #-}
module ClickHaskell.Native where

import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.ByteString.Internal
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text as Text (Text, unpack)
import Data.Word 
import Foreign (Ptr, nullPtr, Storable (..), Bits (shiftL), shiftR, plusPtr, malloc)
import GHC.Generics (Generic)
import Network.Socket
import System.Timeout (timeout)

import ClickHaskell.DbTypes
import Data.Foldable (forM_)
import Control.Monad (foldM)

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
      (Just $ Text.unpack chHost)
      (Just $ Text.unpack chPort)

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
  , chHost     :: Text
  , chPort     :: Text
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
  serialize :: chType -> Ptr Word8

instance Serializable ChUInt64 where
  serialize = accursedUnutterablePerformIO . asPtrWord8 . fromChType @ChUInt64 @Word64

asPtrWord8 :: (Storable a, Bits a, Integral a) => a -> IO (Ptr Word8)
asPtrWord8 storableBits = do
  pointer <- malloc
  foldM
    (\ptr index -> do
      pokeElemOff
        ptr
        index
        (fromIntegral $ storableBits `shiftR` (8 * index))
      pure ptr
    )
    pointer
    [0 .. sizeOf storableBits - 1]


f :: IO ()
f = do
  let storable = maxBound - 256 :: Word64
  ptr8 <- asPtrWord8 storable
  forM_  [0 .. sizeOf storable - 1] (\index -> print @Word8 =<< peekByteOff ptr8 index) 
