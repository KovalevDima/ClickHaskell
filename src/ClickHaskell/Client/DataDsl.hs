{-# LANGUAGE
    AllowAmbiguousTypes
  , FlexibleContexts
  , MonoLocalBinds
  , TypeApplications
  , ScopedTypeVariables
#-}
{-# LANGUAGE RankNTypes #-}

module ClickHaskell.Client.DataDsl where

-- Internal dependencies
import ClickHaskell.Client.Interface (HttpChClient(..), ChException (..))
import ClickHaskell.DataDsl          (InsertableInto (..), SelectableFrom (..), tsvInsertQueryHeader, renderSelectQuery, SelectionDescription)
import ClickHaskell.TableDsl         (IsLocatedTable)

-- External dependenices
import Conduit              (yield, yieldMany)
import Network.HTTP.Client  as H (Request(..), Response(..), httpLbs, RequestBody(..))
import Network.HTTP.Conduit as H (requestBodySourceChunked)
import Network.HTTP.Types   as H (Status(..))

-- GHC included libraries imports
import Control.Exception          (throw)
import Control.Monad              (when)
import Data.ByteString            as BS (toStrict)
import Data.ByteString.Lazy       as BSL (ByteString)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.Text.Encoding         as T (encodeUtf8, decodeUtf8)


httpStreamChSelect :: forall table descripion .
  ( SelectableFrom table descripion
  ) => HttpChClient -> SelectionDescription table descripion -> IO [descripion]
httpStreamChSelect (HttpChClient man req) descConstructor = do
  resp <- H.httpLbs req{H.requestBody = H.RequestBodyBS (renderSelectQuery descConstructor)} man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure
    . map (deserializeSelectionResult @table @descripion . BS.toStrict)
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
        >> yieldMany (map (toInsertableInto @locatedTable) schemaList)
      }
    man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure resp
