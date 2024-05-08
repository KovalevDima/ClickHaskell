{-# LANGUAGE
    AllowAmbiguousTypes
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeApplications
  , UndecidableInstances
  , ScopedTypeVariables
#-}

module Main 
  ( main
  ) where


-- Internal
import ClickHouse.DbTypes 
  ( Deserializable(..), IsChType(..), ToChType(..), ToQueryPart(..)
  , ChUInt64, ChInt64, ChUInt32, ChInt32
  , ChString
  )
import ClickHaskell.Client
  ( ClientInterpretable(..)
  , HttpChClient(..)
  , IsChClient(..)
  , throwOnNon200
  )
import Examples (exampleCredentials)


-- External
import Network.HTTP.Client as H (httpLbs, responseBody, Request(..), RequestBody(..))


-- GHC included
import Control.Monad (when)
import Data.ByteString         as BS (takeWhile, singleton)
import Data.ByteString.Builder as BS (toLazyByteString)
import Data.ByteString.Lazy    as BSL (toStrict)
import Data.Proxy   (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)


main :: IO ()
main = do
  client <-
    initClient
      @HttpChClient
      exampleCredentials
      Nothing

  runSerializationTest @ChInt32 client
  runSerializationTest @ChInt64 client
  runSerializationTest @ChUInt32 client
  runSerializationTest @ChUInt64 client
  -- runSerializationTest @ChString client




runSerializationTest ::
  forall chType
  .
  ( ToQueryPart chType
  , Eq chType
  , Deserializable chType
  , HasTestValues chType
  , KnownSymbol (ToChTypeName chType)
  , Show chType
  )
  =>
  HttpChClient -> IO ()
runSerializationTest client = do
  let runTest = interpretClient @(DeSerializationTest chType) client
  
  deserializationResult <- mapM runTest testValues
  
  print (chTypeName @chType <> ": Ok")




data DeSerializationTest chType

instance
  ( ToQueryPart chType
  , Deserializable chType
  , Eq chType
  , Show chType
  , KnownSymbol (ToChTypeName chType)
  )
  =>
  ClientInterpretable (DeSerializationTest chType) HttpChClient
  where
  type ClientIntepreter (DeSerializationTest chType) = chType -> IO chType
  interpretClient (MkHttpChClient man req) chType = do
    resp
      <- H.httpLbs
        req
          { requestBody
              = H.RequestBodyLBS
              . BS.toLazyByteString
              $ "SELECT " <> toQueryPart chType
          }
        man

    throwOnNon200 resp

    let deserializedChType = deserialize . BS.takeWhile (/= 10) . (BSL.toStrict . H.responseBody) $ resp
    
    (when (chType /= deserializedChType) . error)
      (  "Deserialized value of type " <> show (chTypeName @chType) <> " unmatched:"
      <> " Expected: " <> show chType
      <> ". But got: " <> show deserializedChType <> "."
      )

    pure deserializedChType


class HasTestValues chType
  where
  expectedResult :: [chType]
  expectedResult = testValues

  testValues :: [chType]

instance {-# OVERLAPPABLE #-}
  ( Bounded boundedEnum
  , Enum boundedEnum
  )
  =>
  HasTestValues boundedEnum
  where
  testValues :: [boundedEnum]
  testValues = [minBound, toEnum 0, maxBound]

instance HasTestValues ChString
  where
  testValues = map (toChType . BS.singleton) [1..255]
