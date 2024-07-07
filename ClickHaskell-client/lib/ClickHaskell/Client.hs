{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , OverloadedStrings
  , TypeFamilyDependencies
  , RankNTypes
#-}
module ClickHaskell.Client
  ( module ClickHaskell.Client
  , WritableInto(..)
  , ReadableFrom(..)
  ) where

-- Internal
import ClickHaskell.Internal.Generics (WritableInto(..), ReadableFrom(..))
import ClickHaskell.Tables (Table, Columns, View, renderView)

-- External
import Network.HTTP.Client as H (Request(..), Response(..), RequestBody(..), parseRequest, withResponse, brConsume, BodyReader, Manager)
import Network.HTTP.Types  as H (Status(..))

-- GHC included
import Control.DeepSeq         (NFData)
import Control.Exception       (Exception, SomeException, throw)
import Data.ByteString         as BS (StrictByteString, empty)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8   as BS8 (lines, pack)
import Data.ByteString.Lazy    as BL (toChunks)
import Data.IORef              (newIORef, readIORef, writeIORef)
import Data.Text               as T (Text, unpack)
import Data.Text.Encoding      as T (decodeUtf8, encodeUtf8)
import Data.Typeable           (Proxy (..))
import GHC.Generics            (Generic)
import GHC.IO                  (unsafeInterleaveIO)
import GHC.TypeLits            (KnownSymbol, symbolVal)


-- ToDo: Move into ClickHaskell-http-client

insertInto ::
  forall table record name columns
  .
  ( WritableInto table record
  , KnownSymbol name
  , table ~ Table name columns
  )
  => Manager -> ChCredential -> [record] -> IO ()
insertInto manager cred writingData = do
  insertIntoHttpGeneric
    @Request
    @(Response BodyReader)
    cred
    ("INSERT INTO " <> (byteString . BS8.pack) (symbolVal $ Proxy @name) <> " (" <> writingColumns @table @record <> ") FORMAT TSV\n")
    (toTsvLine @table @record)
    writingData
    (`withResponse` manager)

selectFrom ::
  forall table record name columns
  .
  ( ReadableFrom table record
  , KnownSymbol name
  , table ~ Table name columns
  )
  => Manager -> ChCredential -> IO [record]
selectFrom manager cred =
  selectFromHttpGeneric
    @Request
    @(Response BodyReader)
    @record
    cred
    ("SELECT " <> readingColumns @table @record <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name) <> " FORMAT TSV\n")
    (fromTsvLine @table @record)
    (`withResponse` manager)

selectFromTableFunction ::
  forall tableFunction record name columns parameters
  .
  ( ReadableFrom tableFunction record
  , tableFunction ~ View name columns parameters
  )
  => Manager -> ChCredential -> View name columns '[] -> IO [record]
selectFromTableFunction manager cred tableFuncton =
  selectFromHttpGeneric
    @Request
    @(Response BodyReader)
    @record
    cred
    ("SELECT " <> readingColumns @tableFunction @record <> " FROM " <> renderView tableFuncton <> " FORMAT TSV\n")
    (fromTsvLine @tableFunction @record)
    (`withResponse` manager)

select ::
  forall columnsWrapper record columns
  .
  ( ReadableFrom columnsWrapper record
  , columnsWrapper ~ Columns columns
  )
  => Manager -> ChCredential -> Builder -> IO [record]
select manager cred query =
  selectFromHttpGeneric
    @Request
    @(Response BodyReader)
    @record
    cred
    (query <> " FORMAT TSV\n")
    (fromTsvLine @(Columns columns) @record)
    (`withResponse` manager)

runStatement :: Manager -> ChCredential -> Builder -> IO StrictByteString
runStatement manager chCred statement = injectStatementToRequest
  @Request
  @(Response BodyReader)
  statement
  (either throw id $ initAuthorizedRequest @Request @(Response BodyReader) chCred)
  `withResponse` manager $ fmap mconcat . brConsume . responseBody

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

  injectStatementToRequest query request = request{
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

  -- ToDo: This implementation reads whole body before parsing
  injectReadingToResponse decoder = fmap (decoder . mconcat) . brConsume . unsafeInterleaveIO . responseBody

  injectWritingToRequest query dataList encoder request = request{
    requestBody = RequestBodyStreamChunked $ \np -> do
      writingData <- newIORef . BL.toChunks . toLazyByteString . mconcat . (query:) . map encoder $ dataList
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
      (map decoder . BS8.lines)
      response


-- * Clients abstraction

{- |
Clients initialization abstraction for different backends
-}
class ImpliesClickHouseHttp request response
  where
  initAuthorizedRequest :: ChCredential -> Either SomeException request

  injectStatementToRequest :: Builder -> (request -> request)

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
