{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , OverloadedStrings
  , TypeFamilyDependencies
#-}

module ClickHaskell.Client
(
-- * Client
-- ** Interpreter
  ClientInterpretable(..)

-- ** Reading
, Reading

-- ** Writing
, Writing


-- * Initialization
, IsChClient(..)
, ChCredential(..)

-- ** HTTP client
, HttpChClient(..)
, HttpChClientSettings
, setHttpClientTimeout

-- ** HTTP codes handling
, ChException(..)
, throwOnNon200
) where


-- Internal
import ClickHaskell.Generics (WritableInto(..), ReadableFrom(..))
import ClickHaskell.Tables   (InterpretableTable(..), Table, renderTable, View, renderView)


-- External
import Conduit                     (yield, yieldMany)
import Network.HTTP.Client         as H (httpLbs, newManager, Manager, ManagerSettings(..), Request(..), Response(..), RequestBody(..))
import Network.HTTP.Client.Conduit as H (defaultManagerSettings, parseRequest, responseTimeoutNone, responseTimeoutMicro)
import Network.HTTP.Conduit        as H (requestBodySourceChunked)
import Network.HTTP.Simple         as H (getResponseBody)
import Network.HTTP.Types          as H (Status(..))


-- GHC included
import Control.DeepSeq            (NFData)
import Control.Exception          (throw, Exception)
import Control.Monad              (when)
import Data.ByteString            as BS (toStrict)
import Data.ByteString.Builder    (toLazyByteString)
import Data.ByteString.Lazy       (LazyByteString)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.Kind                  (Type)
import Data.Maybe                 (fromMaybe)
import Data.Text                  as T (Text, unpack)
import Data.Text.Encoding         as T (encodeUtf8, decodeUtf8)
import GHC.Generics               (Generic)


-- * Client language parts

class
  ( IsChClient client
  ) =>
  ClientInterpretable expression client
  where
  type ClientIntepreter expression :: Type
  interpretClient :: client -> ClientIntepreter expression




-- ** Reading

{- |
Declaring reading client operation part.

@
runnableClient :: HttpChClient -> IO [ReadingData]
runnableClient client =
  interpretClient
    \@(Reading ReadingData -> MyTable)
    client
    [exampleDataSample]

data ReadingData = MkReadingData
  { column :: Int64
  } deriving (Generic)

type MyTable = Table "myTable" '[Column "column" ChInt64]
@
-}
data Reading record


instance
  ( ReadableFrom (Table name columns) record
  , InterpretableTable (Table name columns)
  ) =>
  ClientInterpretable (Reading record -> Table name columns) HttpChClient
  where
  type ClientIntepreter (Reading record -> Table name columns) = IO [record]
  interpretClient (MkHttpChClient man req) = do
    resp <- H.httpLbs
      req{H.requestBody = H.RequestBodyBS . BS.toStrict . toLazyByteString
        $  "SELECT " <> readingColumns @(Table name columns) @record
        <> " FROM " <> renderTable (interpretTable @(Table name columns))
      }
      man

    throwOnNon200 resp

    pure $
      ( map (fromTsvLine @(Table name columns) @record . BS.toStrict)
      . BSL8.lines . getResponseBody
      $ resp
      )


instance
  ( ReadableFrom (View name columns params) record
  , InterpretableTable (View name columns params)
  ) =>
  ClientInterpretable (Reading record -> View name columns params) HttpChClient
  where
  type ClientIntepreter (Reading record -> View name columns params)
    = View name columns '[] -> IO [record]
  interpretClient (MkHttpChClient man req) view = do
    resp <- H.httpLbs
      req{H.requestBody = H.RequestBodyBS . BS.toStrict . toLazyByteString
        $  "SELECT " <> readingColumns @(View name columns params) @record
        <> " FROM " <> renderView view
      }
      man

    throwOnNon200 resp

    pure
      ( map (fromTsvLine @(View name columns params) @record . BS.toStrict)
      . BSL8.lines . getResponseBody
      $ resp
      )




-- ** Writing

data Writing record

instance
  ( WritableInto (Table name columns) record
  , InterpretableTable (Table name columns)
  ) =>
  ClientInterpretable (Writing record -> Table name columns) HttpChClient
  where
  type ClientIntepreter (Writing record -> Table name columns)
    = [record] -> IO ()
  interpretClient (MkHttpChClient man req) schemaList = do
    resp <- H.httpLbs
      req{requestBody = H.requestBodySourceChunked $
        do
        yield
          ( BS.toStrict . toLazyByteString
            $  "INSERT INTO " <> renderTable (interpretTable @(Table name columns))
            <> " (" <> writingColumns @(Table name columns) @record <> ")"
            <> " FORMAT TSV\n"
          )
        yieldMany
          ( map
            ( BS.toStrict . toLazyByteString
            . toTsvLine @(Table name columns) @record
            )
            schemaList
          )
      }
      man

    throwOnNon200 resp








-- * Clients abstraction

{- |
Clients initialization abstraction for different backends
-}
class IsChClient client
  where
  type ClientSettings client = settings | settings -> client
  initClient :: ChCredential -> Maybe (ClientSettings client -> ClientSettings client) -> IO client

{- | ToDocument
-}
data ChCredential = MkChCredential
  { chLogin    :: !Text
  , chPass     :: !Text
  , chUrl      :: !Text
  , chDatabase :: !Text
  }
  deriving (Generic, NFData, Show, Eq)




-- ** HTTP ClickHouse client realization

data HttpChClient = MkHttpChClient H.Manager H.Request

instance IsChClient HttpChClient where
  type ClientSettings HttpChClient = HttpChClientSettings
  initClient (MkChCredential login pass url databaseName) maybeModifier = do
    man <- H.newManager $ fromMaybe id maybeModifier defaultHttpChClientSettings
    req <- H.parseRequest (T.unpack url)

    pure $! MkHttpChClient
      man
      req
        { H.method         = "POST"
        , H.requestHeaders =
          [ ("X-ClickHouse-User", encodeUtf8 login)
          , ("X-ClickHouse-Key", encodeUtf8 pass)
          , ("X-ClickHouse-Database", encodeUtf8 databaseName)
          ]
          <> H.requestHeaders req
        }

{- |
Settings for HttpChClient

Required for initialization

Currently is a type synomym for http-client ManagerSettings
-}
type HttpChClientSettings = H.ManagerSettings

defaultHttpChClientSettings :: HttpChClientSettings
defaultHttpChClientSettings = H.defaultManagerSettings{managerResponseTimeout = H.responseTimeoutNone}

setHttpClientTimeout :: Int -> HttpChClientSettings -> HttpChClientSettings
setHttpClientTimeout msTimeout manager = manager{managerResponseTimeout=H.responseTimeoutMicro msTimeout}




-- ** HTTP codes handling

{- | ToDocument
-}
newtype ChException = MkChException
  { exceptionMessage :: Text
  }
  deriving (Show)
  deriving anyclass (Exception)

{- | Unexported

Throws an ChException when got non-200 status code
-}
throwOnNon200 :: Response LazyByteString -> IO ()
throwOnNon200 resp =
  when
    (H.statusCode (responseStatus resp) /= 200)
    (throw . MkChException . (T.decodeUtf8 . BS.toStrict . responseBody) $ resp)
