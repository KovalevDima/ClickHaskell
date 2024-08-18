{-# LANGUAGE
    AllowAmbiguousTypes
  , DerivingStrategies
  , DeriveAnyClass
  , DeriveGeneric
  , RankNTypes
#-}

module ClickHaskell.HTTP where

-- GHC included
import Control.DeepSeq         (NFData)
import Control.Exception       (Exception, SomeException, throw)
import Data.ByteString         as BS (StrictByteString)
import Data.ByteString.Builder (Builder)
import Data.Text               as T (Text)
import GHC.Generics            (Generic)

-- ToDo: Move it into internal ClickHaskell-HTTP package
insertIntoHttpGeneric
  :: forall request response record
  .  ImpliesClickHouseHttp request response
  => ChCredential
  -> Builder
  -> (record -> Builder)
  -> [record]
  -> (request -> (forall result. (response -> IO result) -> IO result))
  -> IO ()
insertIntoHttpGeneric credential query encoder records runClient = injectWritingToRequest
  @request
  @response
  query
  records
  encoder
  (either throw id $ initAuthorizedRequest @request @response credential)
  `runClient` \response -> do
    _ <- throwOnNon200 @request response
    pure ()

selectFromHttpGeneric
  :: forall request response record
  .  ImpliesClickHouseHttp request response
  => ChCredential
  -> Builder
  -> (StrictByteString -> record)
  -> (request -> (forall result . (response -> IO result) -> IO result))
  -> IO [record]
selectFromHttpGeneric credential query decoder runClient =
  (injectStatementToRequest @request @response query . either throw id)
  (initAuthorizedRequest @request @response credential)
  `runClient` \response -> do
    _ <- throwOnNon200 @request response
    injectReadingToResponse
      @request
      @response
      decoder
      response


-- * Clients abstraction

{- |
Clients initialization abstraction for different backends
-}
class ImpliesClickHouseHttp request response
  where
  initAuthorizedRequest :: ChCredential -> Either SomeException request

  injectStatementToRequest :: Builder -> (request -> request)

  injectReadingToResponse :: (StrictByteString -> record) -> (response -> IO [record])

  injectWritingToRequest :: Builder -> [rec] -> (rec -> Builder) -> (request -> request)

  throwOnNon200 :: response -> IO response

{- | ToDocument
-}
data ChCredential = MkChCredential
  { chLogin    :: !Text
  , chPass     :: !Text
  , chUrl      :: !Text
  , chDatabase :: !Text
  }
  deriving (Generic, NFData, Show, Eq)

{- | ToDocument
-}
newtype ChException = MkChException
  { exceptionMessage :: Text
  }
  deriving (Show)
  deriving anyclass (Exception)
