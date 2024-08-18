{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , LambdaCase
  , OverloadedStrings
  , TypeFamilyDependencies
  , RankNTypes
#-}

{-# OPTIONS_GHC
  -Wno-orphans
#-}

module ClickHaskell.Client
  ( select
  , selectFrom
  , selectFromTableFunction

  , insertInto

  , runStatement

  , WritableInto(..)
  , ReadableFrom(..)

  , ChCredential(..)
  ) where

-- Internal
import ClickHaskell.Writing (WritableInto(..))
import ClickHaskell.Reading (ReadableFrom(..))
import ClickHaskell.Tables (Table, View, ParametersInterpreter, CheckParameters, parameters)
import ClickHaskell.HTTP (ImpliesClickHouseHttp(..), ChCredential(..), ChException(..), insertIntoHttpGeneric, selectFromHttpGeneric)

-- External
import Network.HTTP.Client as H (Request(..), Response(..), RequestBody(..), parseRequest, withResponse, brConsume, BodyReader, Manager)
import Network.HTTP.Types  as H (Status(..))

-- GHC included
import Control.Exception       (throw)
import Data.ByteString            as BS (StrictByteString, empty, toStrict)
import Data.ByteString.Builder    (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8      as BS8 (pack)
import Data.ByteString.Lazy       as BSL (toChunks, fromChunks)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.IORef              (newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.Kind               (Type)
import Data.Text               as T (unpack)
import Data.Text.Encoding      as T (decodeUtf8, encodeUtf8)
import Data.Typeable           (Proxy (..))
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
  forall tableFunction record name columns parameters passedParameters
  .
  ( ReadableFrom tableFunction record
  , KnownSymbol name
  , tableFunction ~ View name columns parameters
  , CheckParameters parameters passedParameters
  )
  => Manager -> ChCredential -> (ParametersInterpreter '[] -> ParametersInterpreter passedParameters) -> IO [record]
selectFromTableFunction manager cred interpreter =
  selectFromHttpGeneric
    @Request
    @(Response BodyReader)
    @record
    cred
    ( "SELECT " <> readingColumns @tableFunction @record <>
      " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> parameters interpreter <>
      " FORMAT TSV\n")
    (fromTsvLine @tableFunction @record)
    (`withResponse` manager)

select ::
  forall (columns :: [Type]) record
  .
  ReadableFrom columns record
  => Manager -> ChCredential -> Builder -> IO [record]
select manager cred query =
  selectFromHttpGeneric
    @Request
    @(Response BodyReader)
    @record
    cred
    (query <> " FORMAT TSV\n")
    (fromTsvLine @columns @record)
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
      ibss <- newIORef $ (BSL.toChunks . toLazyByteString) query
      np $ do
        bss <- readIORef ibss
        case bss of
          [] -> return BS.empty
          bs:bss' -> do
            writeIORef ibss bss'
            return bs
  }

  -- ToDo: This implementation reads whole body before parsing
  injectReadingToResponse decoder = fmap (map (decoder . toStrict) . BSL8.lines  . BSL.fromChunks) . brConsume . unsafeInterleaveIO . responseBody

  injectWritingToRequest query dataList encoder request = request{
    requestBody = RequestBodyStreamChunked $ \np -> do
      writingData <- newIORef . BSL.toChunks . toLazyByteString . mconcat . (query:) . map encoder $ dataList
      np . atomicModifyIORef writingData $ \case
        [] -> ([], BS.empty)
        x:xs -> (xs, x)
  }

  throwOnNon200 resp = do
    if H.statusCode (responseStatus resp) /= 200
      then throw . MkChException . T.decodeUtf8 . mconcat =<< (brConsume . responseBody) resp
      else pure resp
