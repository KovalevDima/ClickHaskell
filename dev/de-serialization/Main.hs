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
  ( Deserializable(..), IsChType(..), ToQueryPart(..)
  , ChInt64, ChInt32, ChUInt32, ChUInt64
  )
import ClickHaskell.Client (ClientInterpretable(..), HttpChClient(..), IsChClient(..))
import Examples (exampleCredentials)


-- External
import Network.HTTP.Client as H (httpLbs, responseBody, Request(..), RequestBody(..))


-- GHC included
import Control.Monad (when)
import Data.ByteString.Lazy    as BS (toStrict)
import Data.ByteString.Builder as BS (toLazyByteString)
import Data.Proxy   (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)


main :: IO ()
main = do
  client <- initClient @HttpChClient
    exampleCredentials
    Nothing

  runSerializationTest @ChInt32 client
  runSerializationTest @ChInt64 client
  runSerializationTest @ChUInt32 client
  runSerializationTest @ChUInt64 client




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
  let runTest = interpretClient @(DeSerializationTest chType) client
  deserializationResult <- mapM runTest testValues
  
  let dataTypeName = symbolVal (Proxy @(ToChTypeName chType))
  
  when (deserializationResult /= testValues)
    (error $ "Deserialization bug occured on DataType " <> dataTypeName)
  
  putStrLn $ dataTypeName <> ": Ok"




data DeSerializationTest chType

instance
  ( ToQueryPart chType
  , Deserializable chType
  ) =>
  ClientInterpretable (DeSerializationTest chType) HttpChClient
  where
  type ClientIntepreter (DeSerializationTest chType) = chType -> IO chType
  interpretClient (HttpChClient man req) chType = do
    responseBody
      <-  BS.toStrict . H.responseBody
      <$> H.httpLbs
        req
          { requestBody
              = H.RequestBodyLBS
              . BS.toLazyByteString
              $ "SELECT " <> toQueryPart chType
          }
        man
    pure $ deserialize responseBody




class HasTestValues chType
  where
  expectedResult :: [chType]
  expectedResult = testValues

  testValues :: [chType]

instance
  ( Bounded boundedEnum
  , Enum boundedEnum
  ) =>
  HasTestValues boundedEnum
  where
  testValues :: [boundedEnum]
  testValues = [pred minBound, minBound, toEnum 0, maxBound, succ maxBound]
