{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , OverloadedStrings
  , TypeFamilyDependencies
#-}
module ClickHaskell.ClientV2 where

-- Internal
import ClickHaskell.Generics (WritableInto(..), ReadableFrom(..))
import ClickHaskell.Tables   (InterpretableTable(..), Table, renderTable, View, renderView)


-- External
import Network.HTTP.Client as H (Request(..), Response(..), RequestBody(..), parseRequest)
import Network.HTTP.Types  as H (Status(..))


-- GHC included
import Control.DeepSeq         (NFData)
import Control.Exception       (throw, Exception, SomeException)
import Control.Monad           (when)
import Data.ByteString         as BS (toStrict, empty)
import Data.ByteString.Builder (toLazyByteString, Builder)
import Data.ByteString.Lazy    as BL (LazyByteString, toChunks)
import Data.IORef              (newIORef, readIORef, writeIORef)
import Data.Text               as T (Text, unpack)
import Data.Text.Encoding      as T (encodeUtf8, decodeUtf8)
import GHC.Generics            (Generic)


httpWriting ::
  forall request response table record
  .
  ( ImpliesClickHouseHttp request response
  , WritableInto table record
  , InterpretableTable table
  )
  =>
  ChCredential -> Builder -> [record] -> (request -> IO response) -> IO ()
httpWriting credential tableCall records runClient =
  throwOnNon200 @request
    =<< runClient (
      injectWritingToRequest
        @request 
        @response
        ("INSERT " <> (writingColumns @table @record) <> " INTO " <> tableCall)
        (mconcat $ map (toTsvLine @table) records)
        (either throw id $ initAuthorizedRequest @request @response credential)
      )

-- * Clients abstraction

{- |
Clients initialization abstraction for different backends
-}
class ImpliesClickHouseHttp request response
  where
  initAuthorizedRequest :: ChCredential -> Either SomeException request
  injectWritingToRequest :: Builder -> Builder -> (request -> request)
  throwOnNon200 :: response -> IO ()

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




-- ** HTTP ClickHouse client realization

instance ImpliesClickHouseHttp H.Request (H.Response LazyByteString) where
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

  injectWritingToRequest query writableData request = request{
    requestBody = RequestBodyStreamChunked $ \np -> do
      ibss <- newIORef $ BL.toChunks $ toLazyByteString (query <> writableData)
      np $ do
        bss <- readIORef ibss
        case bss of
          [] -> return BS.empty
          bs:bss' -> do
            writeIORef ibss bss'
            return bs
  }

  throwOnNon200 resp =
    when
      (H.statusCode (responseStatus resp) /= 200)
      (throw . MkChException . (T.decodeUtf8 . BS.toStrict . responseBody) $ resp)
