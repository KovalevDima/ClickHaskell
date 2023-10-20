{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , FunctionalDependencies
  , OverloadedStrings
  , TypeSynonymInstances
#-}

module ClickHaskell.Client.Interface where

-- GHC included libraries imports
import Control.DeepSeq    (NFData)
import Data.Maybe         (fromMaybe)
import Data.Text          as T (Text, unpack)
import Data.Text.Encoding as T (encodeUtf8)
import GHC.Generics       (Generic)
import Control.Exception  (Exception)

-- External dependencies
import Network.HTTP.Client         as H (newManager, Manager, ManagerSettings(..))
import Network.HTTP.Client.Conduit as H (Request(..), defaultManagerSettings, parseRequest, responseTimeoutNone, responseTimeoutMicro)
import Network.HTTP.Simple         as H (setRequestManager)


class ChClient backend connectionsManagerSettings | backend -> connectionsManagerSettings where
  initClient :: ChCredential -> Maybe connectionsManagerSettings -> IO backend

data ChException = ChException
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
