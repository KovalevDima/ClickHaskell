{-# LANGUAGE
    AllowAmbiguousTypes
  , DeriveAnyClass
  , DeriveFunctor
  , DeriveGeneric
  , FlexibleContexts
  , FunctionalDependencies
  , MonoLocalBinds
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeSynonymInstances
#-}
module ClickHaskell.Client where

-- Internal dependencies
import ClickHaskell.DataDsl (InsertableInto (..), SelectableFrom (..), tsvInsertQueryHeader, renderSelectQuery, SelectionDescription)

-- GHC included libraries imports
import Control.DeepSeq            (NFData)
import Control.Exception          (throw, Exception)
import Control.Monad              (when)
import Data.ByteString            as BS (toStrict, ByteString)
import Data.ByteString.Char8      as BS8 (split, notElem, readInteger)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.Maybe                 (fromMaybe)
import Data.Text                  as T (Text, unpack)
import Data.Text.Encoding         as T (encodeUtf8, decodeUtf8)
import GHC.Generics               (Generic)

-- External dependencies
import Conduit                     (yield, yieldMany)
import Network.HTTP.Client         as H (newManager, Manager, ManagerSettings(..), Request(..), Response(..), httpLbs, RequestBody(..))
import Network.HTTP.Client.Conduit as H (defaultManagerSettings, parseRequest, responseTimeoutNone, responseTimeoutMicro)
import Network.HTTP.Conduit        as H (requestBodySourceChunked)
import Network.HTTP.Simple         as H (setRequestManager, getResponseBody, getResponseHeaders)
import Network.HTTP.Types          as H (Status(..))




-- >>> parseSummary summaryExample
-- MkProfileData {readRows = 1, readBytes = 78, writtenRows = 1, writtenBytes = 78, totalRowsToRead = 0, resultRows = 1, resultBytes = 78}
summaryExample :: BS.ByteString
summaryExample =
  "{ \"read_rows\":\"1\"\
  \, \"read_bytes\":\"78\"\
  \, \"written_rows\":\"1\"\
  \, \"written_bytes\":\"78\"\
  \, \"total_rows_to_read\":\"0\"\
  \, \"result_rows\":\"1\"\
  \, \"result_bytes\":\"78\"\
  \}"

parseSummary :: BS.ByteString -> ProfileData
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
  reduce :: ProfileData -> [(BS.ByteString, Integer)] -> ProfileData
  reduce acc (("read_rows"         , integer):xs) = reduce acc{readRows       =integer} xs
  reduce acc (("read_bytes"        , integer):xs) = reduce acc{readBytes      =integer} xs
  reduce acc (("written_rows"      , integer):xs) = reduce acc{writtenRows    =integer} xs
  reduce acc (("written_bytes"     , integer):xs) = reduce acc{writtenBytes   =integer} xs
  reduce acc (("total_rows_to_read", integer):xs) = reduce acc{totalRowsToRead=integer} xs
  reduce acc (("result_rows"       , integer):xs) = reduce acc{resultRows     =integer} xs
  reduce acc (("result_bytes"      , integer):xs) = reduce acc{resultBytes    =integer} xs
  reduce acc [] = acc
  reduce acc ((_, _):xs) = reduce acc xs

  repack :: [BS.ByteString] -> [(BS.ByteString, Integer)]
  repack [] = []
  repack [_] = error "Can't parse json. Please report an issue"
  repack (t1:t2:xs) =
    ( t1
    , case BS8.readInteger t2 of
        Just (int, "") -> int
        Just (_, _) -> error "Can't parse int fully. Please report as issue"
        Nothing -> error "Can't parse int at all. Please report an issue"
    ) : repack xs




data ClientResponse result = MkClientResponse
  { getResult :: result
  , profileData :: ProfileData
  } deriving (Generic)
deriving instance (Show result) => Show (ClientResponse result)
deriving instance Functor ClientResponse


data ProfileData = MkProfileData
  { readRows        :: Integer
  , readBytes       :: Integer
  , writtenRows     :: Integer
  , writtenBytes    :: Integer
  , totalRowsToRead :: Integer
  , resultRows      :: Integer
  , resultBytes     :: Integer
  } deriving (Generic, Show)




httpStreamChSelect :: forall table descripion .
  ( SelectableFrom table descripion
  ) => HttpChClient -> SelectionDescription table descripion -> IO (ClientResponse [descripion])
httpStreamChSelect (HttpChClient man req) descConstructor = do
  resp <- H.httpLbs
    req{H.requestBody = H.RequestBodyBS (renderSelectQuery descConstructor)}
    man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure $
    MkClientResponse
      ( map (fromTsvLine @table @descripion . BS.toStrict)
      . BSL8.lines . getResponseBody
      $ resp
      )
      ( parseSummary
      . fromMaybe (error "Can't find response header \"X-ClickHouse-Summary\". Please report an issue")
      . lookup "X-ClickHouse-Summary" . H.getResponseHeaders
      $ resp
      )




httpStreamChInsert :: forall locatedTable handlingDataDescripion .
  ( InsertableInto locatedTable handlingDataDescripion
  ) => HttpChClient -> [handlingDataDescripion] -> IO (ClientResponse ())
httpStreamChInsert (HttpChClient man req) schemaList = do
  resp <- H.httpLbs
    req
      { requestBody = H.requestBodySourceChunked $
        yield     (encodeUtf8 (tsvInsertQueryHeader @locatedTable @handlingDataDescripion))
        >> yieldMany (map (toTsvLine @locatedTable) schemaList)
      }
    man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure $
    MkClientResponse
      ()
      ( parseSummary
      . fromMaybe (error "Can't find response header \"X-ClickHouse-Summary\". Please report an issue")
      . lookup "X-ClickHouse-Summary" . H.getResponseHeaders
      $ resp
      )




class ChClient backend connectionsManagerSettings | backend -> connectionsManagerSettings where
  initClient :: ChCredential -> Maybe connectionsManagerSettings -> IO backend

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
  initClient (ChCredential login pass url databaseName) mManagerSettings = do
    man <- H.newManager $ fromMaybe H.defaultManagerSettings mManagerSettings
    req <- H.setRequestManager man <$> H.parseRequest (T.unpack url)

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
