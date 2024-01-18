{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveFunctor
  , DeriveGeneric
  , DerivingStrategies
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , TypeFamilyDependencies
  , StandaloneDeriving
  , UndecidableInstances
  , UndecidableSuperClasses
#-}


module ClickHaskell.Client
( -- * Client language parts interpreter
  ClientInterpretable(..)

-- ** Reading
, Reading

-- ** Writing
, Writing




-- * Clients abstraction
, IsChClient(..)
, ChCredential(..)

-- ** HTTP ClickHouse client realization
, HttpChClient(..)
, HttpChClientSettings
, setHttpClientTimeout

-- ** HTTP headers and response wrappers
, ChResponse(..)
, ClickHouseSummary(..)
, parseJsonSummary

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
import Network.HTTP.Simple         as H (getResponseBody, getResponseHeaders)
import Network.HTTP.Types          as H (Status(..))


-- GHC included
import Control.DeepSeq            (NFData)
import Control.Exception          (throw, Exception)
import Control.Monad              (when)
import Data.ByteString            as BS (toStrict, StrictByteString)
import Data.ByteString.Builder    as BS (toLazyByteString)
import Data.ByteString.Char8      as BS8 (split, notElem, readInteger)
import Data.ByteString.Lazy       as BSL (LazyByteString)
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
  ClientInterpretable description client
  where
  type ClientIntepreter description :: Type
  interpretClient :: client -> ClientIntepreter description




-- ** Reading

data Reading record


instance
  ( ReadableFrom (Table name columns) record
  , InterpretableTable (Table name columns)
  ) =>
  ClientInterpretable (Reading record -> Table name columns) HttpChClient
  where
  type ClientIntepreter (Reading record -> Table name columns) = IO (ChResponse [record])
  interpretClient (HttpChClient man req) = do
    resp <- H.httpLbs
      req{H.requestBody = H.RequestBodyBS . BS.toStrict . BS.toLazyByteString
        $  "SELECT " <> readingColumns @(Table name columns) @record
        <> " FROM " <> renderTable (interpretTable @(Table name columns))
      }
      man

    throwOnNon200 resp

    pure $
      MkChResponse
        ( map (fromTsvLine @(Table name columns) @record . BS.toStrict)
        . BSL8.lines . getResponseBody
        $ resp
        )
        (parseSummaryFromHeaders resp)


instance
  ( ReadableFrom (View name columns params) record
  , InterpretableTable (View name columns params)
  , Show record
  ) =>
  ClientInterpretable (Reading record -> View name columns params) HttpChClient
  where
  type ClientIntepreter (Reading record -> View name columns params)
    =  View name columns '[]
    -> IO (ChResponse [record])
  interpretClient (HttpChClient man req) view = do
    resp <- H.httpLbs
      req{H.requestBody = H.RequestBodyBS . BS.toStrict . BS.toLazyByteString
        $  "SELECT " <> readingColumns @(View name columns params) @record
        <> " FROM " <> renderView view
      }
      man

    throwOnNon200 resp

    pure $
      MkChResponse
        ( map (fromTsvLine @(View name columns params) @record . BS.toStrict)
        . BSL8.lines . getResponseBody
        $ resp
        )
        (parseSummaryFromHeaders resp)




-- ** Writing

data Writing record

instance
  ( WritableInto (Table name columns) record
  , InterpretableTable (Table name columns)
  ) =>
  ClientInterpretable (Writing record -> Table name columns) HttpChClient
  where
  type ClientIntepreter (Writing record -> Table name columns)
    = [record] -> IO ClickHouseSummary
  interpretClient (HttpChClient man req) schemaList = do
    resp <- H.httpLbs
      req{requestBody = H.requestBodySourceChunked $
        do
        yield
          ( BS.toStrict . BS.toLazyByteString
            $  "INSERT INTO " <> renderTable (interpretTable @(Table name columns))
            <> " (" <> writingColumns @(Table name columns) @record <> ")"
            <> " FORMAT TSV\n"
          )
        yieldMany
          ( map
            ( BS.toStrict . BS.toLazyByteString
            . toTsvLine @(Table name columns) @record
            )
            schemaList
          )
      }
      man

    throwOnNon200 resp

    pure (parseSummaryFromHeaders resp)








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

data HttpChClient = HttpChClient H.Manager H.Request

instance IsChClient HttpChClient where
  type ClientSettings HttpChClient = HttpChClientSettings
  initClient (MkChCredential login pass url databaseName) maybeModifier = do
    man <- H.newManager $ fromMaybe id maybeModifier defaultHttpChClientSettings
    req <- H.parseRequest (T.unpack url)

    pure $! HttpChClient
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





-- ** HTTP headers and response wrappers

{- |
Wrapper with info from HTTP header @X-ClickHouse-Summary@
-}
data ChResponse result = MkChResponse
  { getResult :: result
  , profileData :: ClickHouseSummary
  } deriving (Generic, Functor)
deriving instance (Show result) => Show (ChResponse result)

{- | ToDocument
-}
data ClickHouseSummary = MkClickHouseSummary
  { readRows        :: Integer
  , readBytes       :: Integer
  , writtenRows     :: Integer
  , writtenBytes    :: Integer
  , totalRowsToRead :: Integer
  , resultRows      :: Integer
  , resultBytes     :: Integer
  } deriving (Generic, Show, Eq)



{- |
1. Takes an HTTP Response
2. Lookups @X-ClickHouse-Summary@ header
3. Parses it
-}
parseSummaryFromHeaders :: Response a -> ClickHouseSummary
parseSummaryFromHeaders
  = parseJsonSummary
  . fromMaybe (error "Can't find response header \"X-ClickHouse-Summary\". Please report an issue")
  . lookup "X-ClickHouse-Summary" . H.getResponseHeaders

{- |
ClickHouseSummary JSON parser. Takes raw byte sequence and parses it
-}
{-# WARNING
  parseJsonSummary
  "Be carefull using this function yourself\n\
  \ It's export allowed for testing\n\
  \ Also it's quite unstable\n"
#-}
parseJsonSummary :: StrictByteString -> ClickHouseSummary
parseJsonSummary
  = reduce (MkClickHouseSummary (-1) (-1) (-1) (-1) (-1) (-1) (-1))
  . repack
  . filter
    (\bs
      -> BS8.notElem '}' bs
      && BS8.notElem '{' bs
      && BS8.notElem ';' bs
      && BS8.notElem ',' bs
      && BS8.notElem ':' bs
    )
  . BS8.split '"'
  where
  reduce :: ClickHouseSummary -> [(StrictByteString, Integer)] -> ClickHouseSummary
  reduce acc (("read_rows"         , integer):xs) = reduce acc{readRows       =integer} xs
  reduce acc (("read_bytes"        , integer):xs) = reduce acc{readBytes      =integer} xs
  reduce acc (("written_rows"      , integer):xs) = reduce acc{writtenRows    =integer} xs
  reduce acc (("written_bytes"     , integer):xs) = reduce acc{writtenBytes   =integer} xs
  reduce acc (("total_rows_to_read", integer):xs) = reduce acc{totalRowsToRead=integer} xs
  reduce acc (("result_rows"       , integer):xs) = reduce acc{resultRows     =integer} xs
  reduce acc (("result_bytes"      , integer):xs) = reduce acc{resultBytes    =integer} xs
  reduce acc [] = acc
  reduce acc ((_, _):xs) = reduce acc xs

  repack :: [StrictByteString] -> [(StrictByteString, Integer)]
  repack [] = []
  repack [_] = error "Can't parse json. Please report an issue"
  repack (t1:t2:xs) =
    ( t1
    , case BS8.readInteger t2 of
        Just (int, "") -> int
        Just (_, _) -> error "Can't parse int fully. Please report as issue"
        Nothing -> error "Can't parse int at all. Please report an issue"
    ) : repack xs




-- ** HTTP codes handling

{- | ToDocument
-}
newtype ChException = ChException
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
    (throw . ChException . (T.decodeUtf8 . BS.toStrict . responseBody) $ resp)
