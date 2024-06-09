{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , OverloadedStrings
  , TypeFamilyDependencies
#-}
module ClickHaskell.ClientV2List where

-- Internal
import ClickHaskell.Generics (WritableInto(..), ReadableFrom(..))

-- External
import Network.HTTP.Client as H (Request(..), Response(..), RequestBody(..), parseRequest, responseOpen, brConsume, BodyReader, Manager, brRead)
import Network.HTTP.Types  as H (Status(..))


-- GHC included
import Control.DeepSeq         (NFData)
import Control.Exception       (throw, Exception, SomeException)
import Data.ByteString         as BS (empty, StrictByteString)
import Data.ByteString.Builder (toLazyByteString, Builder)
import Data.ByteString.Lazy    as BL (toChunks)
import Data.ByteString.Char8   as BS8 (lines)
import Data.IORef              (newIORef, readIORef, writeIORef)
import Data.Text               as T (Text, unpack)
import Data.Text.Encoding      as T (encodeUtf8, decodeUtf8)
import GHC.Generics            (Generic)


-- ToDo: Move into ClickHaskell-http-client

insertInto :: forall table record . WritableInto table record => Manager -> ChCredential -> Builder -> [record] -> IO ()
insertInto manager cred table writingQueue = do
  insertIntoHttpGeneric
    @Request
    @(Response BodyReader)
    cred
    ("INSERT INTO " <> table <> " (" <> writingColumns @table @record <>  ") FORMAT TSV\n")
    (toTsvLine @table @record)
    writingQueue
    (`responseOpen` manager)

selectFrom :: forall table record . ReadableFrom table record => Manager -> ChCredential -> Builder -> IO [record]
selectFrom manager cred table =
  selectFromHttpGeneric
    @Request
    @(Response BodyReader)
    @record
    cred
    ("SELECT " <> readingColumns @table @record <> " FROM " <> table <> " FORMAT TSV\n")
    (fromTsvLine @table @record)
    (`responseOpen` manager)




instance ImpliesClickHouseHttp H.Request (H.Response BodyReader) where
  initAuthorizedRequest (MkChCredential login pass url databaseName) = do
    req <- H.parseRequest (T.unpack url)
    pure $!
      req
        { H.method         = "POST"
        , H.requestHeaders =
          [ ("X-ClickHouse-User", encodeUtf8 login)
          , ("X-ClickHouse-Key", encodeUtf8 pass)
          , ("X-ClickHouse-Database", encodeUtf8 databaseName)
          ]
          <> H.requestHeaders req
        }

  injectReadingToRequest query request = request{
    requestBody = RequestBodyStreamChunked $ \np -> do
      ibss <- newIORef $ (BL.toChunks . toLazyByteString) query
      np $ do
        bss <- readIORef ibss
        case bss of
          [] -> return BS.empty
          bs:bss' -> do
            writeIORef ibss bss'
            return bs
  }
  injectReadingToResponse decoder response = do
    bs <- (brRead . responseBody) response
    print bs
    pure $ decoder bs

  {-# INLINE [0] injectWritingToRequest #-}
  injectWritingToRequest query dataQueue encoder request = request{
    requestBody = RequestBodyStreamChunked $ \np -> do
      writingData <- newIORef . BL.toChunks . toLazyByteString . mconcat . (query:) . map encoder $ dataQueue
      np $ do
        bss <- readIORef writingData
        case bss of
          [] -> return BS.empty
          bs:bss' -> do
            writeIORef writingData bss'
            return bs
  }

  throwOnNon200 resp = do
    if H.statusCode (responseStatus resp) /= 200
      then throw . MkChException . T.decodeUtf8 . mconcat =<< (brConsume . responseBody) resp
      else pure resp








-- ToDo: Move it into internal ClickHaskell-HTTP package

{-# INLINE [0] insertIntoHttpGeneric #-}
insertIntoHttpGeneric ::
  forall request response record
  .
  ImpliesClickHouseHttp request response
  =>
  ChCredential -> Builder -> (record -> Builder) -> [record] -> (request -> IO response) -> IO ()
insertIntoHttpGeneric credential query encoder records runClient = do
  const (pure ())
    =<< throwOnNon200 @request
    =<< runClient (
      injectWritingToRequest
        @request
        @response
        query
        records
        encoder
        (either throw id $ initAuthorizedRequest @request @response credential)
      )

selectFromHttpGeneric ::
  forall request response record
  .
  ImpliesClickHouseHttp request response
  =>
  ChCredential -> Builder -> (StrictByteString -> record) -> (request -> IO response) -> IO [record]
selectFromHttpGeneric credential tableCall decoder runClient =
  injectReadingToResponse
    @request
    @response
    (map decoder . BS8.lines)
    =<< throwOnNon200 @request
    =<< runClient (
      (injectReadingToRequest @request @response tableCall . either throw id)
      (initAuthorizedRequest @request @response credential)
    )


-- * Clients abstraction

{- |
Clients initialization abstraction for different backends
-}
class ImpliesClickHouseHttp request response
  where
  initAuthorizedRequest :: ChCredential -> Either SomeException request

  injectReadingToRequest :: Builder -> (request -> request)
  injectReadingToResponse :: (StrictByteString -> [record]) -> (response -> IO [record])

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
