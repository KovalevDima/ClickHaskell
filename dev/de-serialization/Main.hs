{-# LANGUAGE
    AllowAmbiguousTypes
  , FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeApplications
  , ScopedTypeVariables
#-}

module Main where

import ClickHouse.DbTypes
import ClickHaskell.Client
import Examples (exampleCredentials)

import Data.ByteString.Lazy as BS (toStrict)
import Data.ByteString.Builder as BS (toLazyByteString)
import Control.Monad (when)
import Data.Proxy (Proxy(..))
import GHC.TypeLits

import Network.HTTP.Client as H (httpLbs, responseBody, Request(..), RequestBody(..))

main :: IO ()
main = do
  client <- initClient @HttpChClient
    exampleCredentials
    Nothing

  runSerializationTest @ChInt32 client
  runSerializationTest @ChInt64 client
  runSerializationTest @ChUInt32 client
  runSerializationTest @ChUInt64 client


data TestSerialization chType


class HasTestValues chType where
  testValues :: [chType]

instance HasTestValues ChInt32 where
  testValues = [minBound, minBound+1, -1, 0, 1, maxBound, maxBound+1]

instance HasTestValues ChInt64 where
  testValues = [minBound, minBound+1, -1, 0, 1, maxBound, maxBound+1]

instance HasTestValues ChUInt32 where
  testValues = [minBound, minBound+1, -1, 0, 1, maxBound, maxBound+1]

instance HasTestValues ChUInt64 where
  testValues = [minBound, minBound+1, -1, 0, 1, maxBound, maxBound+1]


runSerializationTest ::
  forall chType
  .
  ( ToQueryPart chType
  , Eq chType
  , Deserializable chType
  , HasTestValues chType
  , KnownSymbol (ToChTypeName chType)
  ) =>
  HttpChClient -> IO ()
runSerializationTest  client = do
  deserializationResult <-
    mapM
      (performOperation @(TestSerialization chType) client)
      testValues
  let dataTypeName = symbolVal (Proxy @(ToChTypeName chType))
  if deserializationResult /= testValues
    then error $ "Deserialization bug occured on DataType " <> dataTypeName
    else putStrLn $ dataTypeName <> ": Ok"


instance
  ( ToQueryPart chType
  , Deserializable chType
  )
  =>
  PerformableOperation (TestSerialization chType) HttpChClient where
  type ClientIntepreter (TestSerialization chType) resp = chType -> IO resp
  type ExpectedDbResponse (TestSerialization chType) = chType
  performOperation (HttpChClient man req) chType = do
    resp <- H.httpLbs
      req
        { requestBody
            = H.RequestBodyLBS
            . BS.toLazyByteString
            $ "SELECT " <> toQueryPart chType
        }
      man
    let deserializedResponseBody = (BS.toStrict . H.responseBody) resp
    pure $ deserialize deserializedResponseBody
