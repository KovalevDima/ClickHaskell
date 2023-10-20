{-# LANGUAGE
    AllowAmbiguousTypes
  , FlexibleContexts
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}

module ClickHaskell.Client.TableDsl where

-- Internal dependencies
import ClickHaskell.Client.Interface (HttpChClient (..), ChException (..))
import ClickHaskell.TableDsl         (IsLocatedTable(..), IsTable(..), showCreateTableIfNotExists)

-- External dependenices
import Network.HTTP.Client as H (Request(..), Response(..), httpLbs, RequestBody (..))
import Network.HTTP.Types  as H (Status(..))

-- GHC included libraries imports
import Control.Exception     (throw)
import Control.Monad         (when)
import Data.ByteString       as BS (toStrict)
import Data.ByteString.Char8 as BS8 (fromStrict, pack)
import Data.Text.Encoding    as T (encodeUtf8, decodeUtf8)
import Data.Proxy            (Proxy(..))
import GHC.TypeLits          (KnownSymbol, symbolVal)



createDatabaseIfNotExists :: forall db . KnownSymbol db => HttpChClient -> IO ()
createDatabaseIfNotExists (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.RequestBodyLBS
      $ "CREATE DATABASE IF NOT EXISTS " <> (BS8.fromStrict . BS8.pack . symbolVal) (Proxy @db)
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp


createTableIfNotExists :: forall locatedTable .
  ( IsLocatedTable locatedTable
  , IsTable locatedTable
  ) => HttpChClient -> IO ()
createTableIfNotExists (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.RequestBodyLBS
      $ BS8.fromStrict . T.encodeUtf8 $ showCreateTableIfNotExists @locatedTable
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp
