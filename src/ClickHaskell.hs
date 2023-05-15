{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , UndecidableInstances
  #-}

{-# OPTIONS_GHC
  -Wno-unrecognised-pragmas
#-}
module ClickHaskell
  ( module ClickHaskell.ChTypes
  , module ClickHaskell.TableDsl

  -- * Data manipulation DSL 
  , httpStreamChInsert, httpStreamChSelect, tsvSelectQuery, tsvInsertQueryHeader, ChException(..)

  -- * Buffered writing abstractions
  , writeToSizedBuffer, createSizedBuffer, readFromSizedBuffer, forkBufferFlusher, BufferSize(..), DefaultBuffer

  -- * Bootstrapping
  , createTableIfNotExists, createDatabaseIfNotExists

  -- * Client abstraction
  , ChClient(initClient), ChCredential(ChCredential, chLogin, chPass, chUrl)
  , HttpChClient, HttpClientSettings, defaultHttpClientSettings, setHttpClientTimeout

  -- Reexports
  , Proxy(..), someSymbolVal, SomeSymbol(..), Generic, Word32, Word64,
  ) where

-- Internal dependencies
import ClickHaskell.ChTypes
import ClickHaskell.TableDsl

-- External dependencies
import Conduit                     (yieldMany, yield)
import Data.Singletons             (SingI)
import Network.HTTP.Client         as H (newManager, Manager, Response, httpLbs, responseStatus, responseBody, RequestBody(..), ManagerSettings(..))
import Network.HTTP.Client.Conduit as H (Request(..), defaultManagerSettings, parseRequest, requestBodySourceChunked, responseTimeoutNone, responseTimeoutMicro)
import Network.HTTP.Simple         as H (setRequestManager)
import Network.HTTP.Types          as H (statusCode)

-- GHC included libraries imports
import Control.Concurrent         (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM     (TBQueue, atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.DeepSeq            (NFData)
import Control.Exception          (Exception, SomeException, handle, throw)
import Control.Monad              (forever, unless, when)
import Data.ByteString            as BS (toStrict)
import Data.ByteString.Lazy       as BSL (ByteString, toStrict)
import Data.ByteString.Lazy.Char8 as BSL8 (lines, pack)
import Data.Data                  (Proxy (..))
import Data.Maybe                 (fromMaybe)
import Data.Text                  as T (Text, intercalate, pack, unpack)
import Data.Text.Encoding         as T (decodeUtf8, encodeUtf8)
import Data.Text.Lazy             as T (toStrict)
import Data.Text.Lazy.Builder     as T (toLazyText)
import Data.Word                  (Word64, Word32)
import GHC.Generics               (Generic)
import GHC.Num                    (Natural)
import GHC.TypeLits               (KnownSymbol, Symbol, symbolVal, someSymbolVal, SomeSymbol (SomeSymbol))


data ChException = ChException
  { exceptionMessage :: Text
  } deriving (Show, Exception)


createDatabaseIfNotExists :: forall db . KnownSymbol db => HttpChClient -> IO ()
createDatabaseIfNotExists (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.RequestBodyLBS
      $ "CREATE DATABASE IF NOT EXISTS " <> (BSL8.pack. symbolVal) (Proxy @db)
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp


createTableIfNotExists :: forall locatedTable db table name columns engine orderBy partitionBy .
  ( table ~ Table name columns engine orderBy partitionBy
  , locatedTable ~ InDatabase db table
  , KnownSymbol name
  , KnownSymbol db
  , SingI partitionBy
  , SingI orderBy
  , SingI (SupportedAndVerifiedColumns columns)
  , IsChEngine engine
  ) => HttpChClient -> IO ()
createTableIfNotExists (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.RequestBodyLBS
      $ BSL8.pack (showCreateTableIfNotExists @locatedTable)
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp


httpStreamChSelect :: forall handlingDataDescripion locatedTable db table name columns engine partitionBy orderBy .
  ( HasChSchema (Unwraped handlingDataDescripion)
  , locatedTable ~ InDatabase db (table (name :: Symbol) columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  , ToConditionalExpression handlingDataDescripion
  )
  => HttpChClient -> IO [Unwraped handlingDataDescripion]
httpStreamChSelect (HttpChClient man req) = do
  resp <- H.httpLbs
    req
      { requestBody = H.requestBodySourceChunked
      $ yield (encodeUtf8 $ tsvSelectQuery @handlingDataDescripion @locatedTable)
      }
    man
  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure
    . map (fromBs . BSL.toStrict)
    . BSL8.lines
    $ responseBody resp

-- ToDo4: Implement table and handling data validation
tsvSelectQuery :: forall
  handlingDataDescripion tableWithDb
  db
  tableWrapper name columns engine partitionBy orderBy
  .
  ( HasChSchema (Unwraped handlingDataDescripion)
  , tableWithDb ~ InDatabase db (tableWrapper name columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  , ToConditionalExpression handlingDataDescripion
  ) => Text
tsvSelectQuery =
  let columnsMapping = T.intercalate "," . map fst $ getSchema @(Unwraped handlingDataDescripion)
      whereConditions = T.toStrict (T.toLazyText $ toConditionalExpression @handlingDataDescripion)
  in  "SELECT " <> columnsMapping
  <> " FROM " <> (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name)
  <> " " <> (if whereConditions=="" then "" else "WHERE " <> whereConditions)
  <> " FORMAT TSV"
{-# INLINE tsvSelectQuery #-}


httpStreamChInsert :: forall locatedTable handlingDataDescripion db table name columns engine partitionBy orderBy .
  ( HasChSchema handlingDataDescripion
  , locatedTable ~ InDatabase db (table (name :: Symbol) columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  ) => HttpChClient -> [handlingDataDescripion] -> IO (H.Response BSL.ByteString)
httpStreamChInsert (HttpChClient man req) schemaList = do
  resp <- H.httpLbs
    req
      { requestBody = H.requestBodySourceChunked $
        yield     (encodeUtf8 $ tsvInsertQueryHeader @handlingDataDescripion @locatedTable)
        >> yieldMany (map toBs schemaList)
      }
    man

  when (H.statusCode (responseStatus resp) /= 200) $
    throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

  pure resp


-- ToDo4: Implement table and handling data validation
tsvInsertQueryHeader :: forall chSchema t db table name columns engine partitionBy orderBy .
  ( HasChSchema chSchema
  , t ~ InDatabase db (table name columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  ) => Text
tsvInsertQueryHeader =
  let columnsMapping = T.intercalate "," . map fst $ getSchema @chSchema
  in "INSERT INTO " <> (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name)
  <> " (" <> columnsMapping <> ")"
  <> " FORMAT TSV\n"
{-# INLINE tsvInsertQueryHeader #-}




-- | Forks buffer flusher with given frequency 
--
forkBufferFlusher :: (HasChSchema schemaData, IsBuffer buffer schemaData)
  => Int                      -- ^ Flushes frequency
  -> buffer schemaData        -- ^ Buffer with schema specialized data
  -> (SomeException -> IO ()) -- ^ Flush action exception handler
  -> ([schemaData] -> IO ())  -- ^ Flush action
  -> IO ThreadId
forkBufferFlusher freq buffer exceptionHandler flushAction
  = forkIO . forever
  $ do
    threadDelay freq
    bufferData <- readFromSizedBuffer buffer
    unless (null bufferData)
      ( handle exceptionHandler
      $ flushAction bufferData
      )

newtype BufferSize = BufferSize Natural deriving newtype Num

class IsBuffer buffer schemaData
  where
  writeToSizedBuffer  :: buffer schemaData -> schemaData -> IO ()
  createSizedBuffer   :: BufferSize -> IO (buffer schemaData)
  readFromSizedBuffer :: buffer schemaData  -> IO [schemaData]


type DefaultBuffer = TBQueue

instance HasChSchema schemaData
  => IsBuffer DefaultBuffer schemaData
  where
  createSizedBuffer   (BufferSize size) = newTBQueueIO size
  writeToSizedBuffer  buffer d          = atomically $ writeTBQueue buffer d
  readFromSizedBuffer buffer            = atomically $ flushTBQueue buffer




class ChClient backend connectionsManagerSettings | backend -> connectionsManagerSettings where
  initClient :: ChCredential -> Maybe connectionsManagerSettings -> IO backend

data ChCredential = ChCredential
  { chLogin :: !Text
  , chPass  :: !Text
  , chUrl   :: !Text
  }
  deriving (Generic, NFData, Show, Eq)




data HttpChClient = HttpChClient H.Manager H.Request

type HttpClientSettings = ManagerSettings

defaultHttpClientSettings :: HttpClientSettings
defaultHttpClientSettings = H.defaultManagerSettings{managerResponseTimeout = H.responseTimeoutNone}

setHttpClientTimeout :: Int -> HttpClientSettings -> HttpClientSettings
setHttpClientTimeout msTimeout manager = manager{managerResponseTimeout=H.responseTimeoutMicro msTimeout}


instance ChClient HttpChClient H.ManagerSettings where
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
