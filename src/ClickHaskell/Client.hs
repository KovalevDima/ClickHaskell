{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , FunctionalDependencies
  , OverloadedStrings
  , TypeSynonymInstances
#-}
{-# LANGUAGE
    AllowAmbiguousTypes
  , FlexibleContexts
  , MonoLocalBinds
  , TypeApplications
  , ScopedTypeVariables
#-}
module ClickHaskell.Client where
-- Internal dependencies

import ClickHaskell.DataDsl          (InsertableInto (..), SelectableFrom (..), tsvInsertQueryHeader, renderSelectQuery, SelectionDescription)
import ClickHaskell.TableDsl         (IsLocatedTable)

-- GHC included libraries imports
import Control.DeepSeq            (NFData)
import Control.Exception          (throw, Exception)
import Control.Monad              (when)
import Data.ByteString            as BS (toStrict)
import Data.ByteString.Lazy       as BSL (ByteString)
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
import Network.HTTP.Simple         as H (setRequestManager)
import Network.HTTP.Types          as H (Status(..))


httpStreamChSelect :: forall table descripion .
  ( SelectableFrom table descripion
  ) => HttpChClient -> SelectionDescription table descripion -> IO [descripion]
httpStreamChSelect (HttpChClient man req) descConstructor = do
  resp <- H.httpLbs req{H.requestBody = H.RequestBodyBS (renderSelectQuery descConstructor)} man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure
    . map (fromTsvLine @table @descripion . BS.toStrict)
    . BSL8.lines
    $ responseBody resp


httpStreamChInsert :: forall locatedTable handlingDataDescripion .
  ( InsertableInto locatedTable handlingDataDescripion
  , IsLocatedTable locatedTable
  ) => HttpChClient -> [handlingDataDescripion] -> IO (H.Response BSL.ByteString)
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

  pure resp




class ChClient backend connectionsManagerSettings | backend -> connectionsManagerSettings where
  initClient :: ChCredential -> Maybe connectionsManagerSettings -> IO backend

newtype ChException = ChException
  { exceptionMessage :: Text
  } deriving (Show, Exception)

data ChCredential = ChCredential
  { chLogin :: !Text
  , chPass  :: !Text
  , chUrl   :: !Text
  }
  deriving (Generic, NFData, Show, Eq)




data HttpChClient = HttpChClient H.Manager H.Request

type HttpClientSettings = H.ManagerSettings

defaultHttpClientSettings :: HttpClientSettings
defaultHttpClientSettings = H.defaultManagerSettings{managerResponseTimeout = H.responseTimeoutNone}

setHttpClientTimeout :: Int -> HttpClientSettings -> HttpClientSettings
setHttpClientTimeout msTimeout manager = manager{managerResponseTimeout=H.responseTimeoutMicro msTimeout}

instance ChClient HttpChClient HttpClientSettings where
  initClient (ChCredential login pass url) mManagerSettings = do
    man <- H.newManager $ fromMaybe H.defaultManagerSettings mManagerSettings
    req <- H.setRequestManager man <$> H.parseRequest (T.unpack url)

    pure $! HttpChClient
      man
      req
        { H.method         = "POST"
        , H.requestHeaders =
          [ ("X-ClickHouse-User", encodeUtf8 login)
          , ("X-ClickHouse-Key", encodeUtf8 pass)
          ]
          <> H.requestHeaders req
        }
