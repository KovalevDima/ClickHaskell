{-# LANGUAGE
    AllowAmbiguousTypes
  , DeriveAnyClass
  , DeriveFunctor
  , DeriveGeneric
  , FunctionalDependencies
  , OverloadedStrings
  , StandaloneDeriving
#-}

module ClickHaskell.Client where

-- Internal dependencies
import ClickHaskell.Operations (ReadableFrom(..), WritableInto(..), IsOperation(..), ClickHouse, Reading, Writing, renderSelectQuery, renderInsertHeader)

-- GHC included libraries imports
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

-- External dependencies
import Conduit                     (yield, yieldMany)
import Network.HTTP.Client         as H (newManager, Manager, ManagerSettings(..), Request(..), Response(..), httpLbs, RequestBody(..))
import Network.HTTP.Client.Conduit as H (defaultManagerSettings, parseRequest, responseTimeoutNone, responseTimeoutMicro)
import Network.HTTP.Conduit        as H (requestBodySourceChunked)
import Network.HTTP.Simple         as H (getResponseBody, getResponseHeaders)
import Network.HTTP.Types          as H (Status(..))




data ChResponse result = MkChResponse
  { getResult :: result
  , profileData :: ProfileData
  } deriving (Generic, Functor)
deriving instance (Show result) => Show (ChResponse result)




data ProfileData = MkProfileData
  { readRows        :: Integer
  , readBytes       :: Integer
  , writtenRows     :: Integer
  , writtenBytes    :: Integer
  , totalRowsToRead :: Integer
  , resultRows      :: Integer
  , resultBytes     :: Integer
  } deriving (Generic, Show, Eq)

parseSummary :: StrictByteString -> ProfileData
parseSummary
  = reduce (MkProfileData (-1) (-1) (-1) (-1) (-1) (-1) (-1))
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
  reduce :: ProfileData -> [(StrictByteString, Integer)] -> ProfileData
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

parseSummaryFromHeaders :: Response a -> ProfileData
parseSummaryFromHeaders
  = parseSummary
  . fromMaybe (error "Can't find response header \"X-ClickHouse-Summary\". Please report an issue")
  . lookup "X-ClickHouse-Summary" . H.getResponseHeaders




throwOnNon200 :: Response LazyByteString -> IO ()
throwOnNon200 resp =
  when
    (H.statusCode (responseStatus resp) /= 200)
    (throw . ChException . (T.decodeUtf8 . BS.toStrict . responseBody) $ resp)




class PerformableOperation description client where
  type ExpectedDbResponse description :: Type
  type ClientIntepreter description dimension :: Type
  performOperation :: client -> ClientIntepreter description (ExpectedDbResponse description)


instance
  ( ReadableFrom table resultData
  ) =>
  PerformableOperation (Reading resultData -> ClickHouse table) HttpChClient
  where
  type ClientIntepreter (Reading resultData -> ClickHouse table) resp = IO resp
  
  type ExpectedDbResponse (Reading resultData -> ClickHouse table) = ChResponse [resultData]
  
  performOperation (HttpChClient man req) = do
    resp <- H.httpLbs
      req{H.requestBody = H.RequestBodyBS
        ( renderSelectQuery
        $ operation @(Reading resultData -> ClickHouse table)
        )
      }
      man

    throwOnNon200 resp

    pure $
      MkChResponse
        ( map (fromTsvLine @table @resultData . BS.toStrict)
        . BSL8.lines . getResponseBody
        $ resp
        )
        (parseSummaryFromHeaders resp)


instance
  ( WritableInto table record
  ) =>
  PerformableOperation (Writing record -> ClickHouse table) HttpChClient
  where
  type ClientIntepreter (Writing record -> ClickHouse table) resp = [record] -> IO resp
  
  type ExpectedDbResponse (Writing record -> ClickHouse table) = ChResponse ()
  
  performOperation (HttpChClient man req) schemaList = do
    resp <- H.httpLbs
      req
        { requestBody = H.requestBodySourceChunked
          $  yield (
            renderInsertHeader
              (operation @(Writing record -> ClickHouse table))
            <> " FORMAT TSV\n"
            )
          >> yieldMany (
            map
              ( BS.toStrict
              . BS.toLazyByteString
              . toTsvLine @table @record
              )
              schemaList
            )
        }
      man

    throwOnNon200 resp

    pure $ MkChResponse () (parseSummaryFromHeaders resp)




class ChClient backend connectionsManagerSettings | backend -> connectionsManagerSettings
  where
  initClient :: ChCredential -> Maybe (connectionsManagerSettings -> connectionsManagerSettings) -> IO backend

newtype ChException = ChException
  { exceptionMessage :: Text
  } deriving (Show, Exception)

data ChCredential = ChCredential
  { chLogin    :: !Text
  , chPass     :: !Text
  , chUrl      :: !Text
  , chDatabase :: !Text
  }
  deriving (Generic, NFData, Show, Eq)




data HttpChClient = HttpChClient H.Manager H.Request

type HttpClientSettings = H.ManagerSettings

defaultHttpClientSettings :: HttpClientSettings
defaultHttpClientSettings = H.defaultManagerSettings{managerResponseTimeout = H.responseTimeoutNone}

setHttpClientTimeout :: Int -> HttpClientSettings -> HttpClientSettings
setHttpClientTimeout msTimeout manager = manager{managerResponseTimeout=H.responseTimeoutMicro msTimeout}

instance ChClient HttpChClient HttpClientSettings where
  initClient (ChCredential login pass url databaseName) maybeModifier = do
    man <- H.newManager $ fromMaybe id maybeModifier H.defaultManagerSettings
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
