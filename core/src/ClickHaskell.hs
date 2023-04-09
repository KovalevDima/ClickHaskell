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
  (
  -- * Data manipulation DSL 
  httpStreamChInsert, httpStreamChSelect, Database(..), Table(..), tsvSelectQuery, tsvInsertQueryHeader, ChException(..)

  -- * Buffered writing abstractions
  , writeToSizedBuffer, createSizedBuffer, readFromSizedBuffer, forkBufferFlusher, BufferSize(..), TBQueue

  -- * Client abstraction
  , ChClient(initClient), HttpChClient, ChCredential(ChCredential, chLogin, chPass, chUrl)
  ) where

import Data.ByteString            as BS (toStrict)
import Data.ByteString.Lazy.Char8 as BSL8 (lines, toStrict)
import Data.ByteString.Lazy       as BSL (ByteString)
import Data.Data                  (Proxy(..))
import Data.Text                  as T (Text, intercalate, unpack, pack)
import Data.Text.Lazy             as T (toStrict)
import Data.Text.Lazy.Builder     as T (toLazyText)
import Data.Text.Encoding         as T (encodeUtf8, decodeUtf8)
import Control.Concurrent         (forkIO, ThreadId, threadDelay)
import Control.Concurrent.STM     (TBQueue, writeTBQueue, atomically, newTBQueueIO, flushTBQueue)
import Control.DeepSeq            (NFData)
import Control.Exception          (SomeException, handle, Exception, throw)
import Control.Monad              (forever, unless)
import GHC.Exts                   (IsString)
import GHC.Generics               (Generic)
import GHC.Num                    (Natural)
import GHC.TypeLits               (symbolVal, KnownSymbol, Symbol)

import Conduit                     (yieldMany, yield)
import Network.HTTP.Client         as H (newManager, Manager, Response, httpLbs, responseStatus, responseBody)
import Network.HTTP.Client.Conduit as H (Request (..), defaultManagerSettings, parseRequest, requestBodySourceChunked)
import Network.HTTP.Simple         as H (setRequestManager)
import Network.HTTP.Types          (statusCode)

import ClickHaskell.TableDsl (HasChSchema (getSchema, fromBs, toBs), InDatabase, Unwraped, toConditionalExpression, ToConditionalExpression)


data ChException = ChException
  { exceptionMessage :: Text
  } deriving (Show, Exception)


newtype Database = Database Text deriving newtype (Show, IsString)
newtype Table    = Table    Text deriving newtype (Show, IsString)


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
  bytestring <- if statusCode (responseStatus resp) == 200
    then pure $ responseBody resp
    else throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp
  pure $ map (fromBs . BSL8.toStrict) $ BSL8.lines bytestring

-- ToDo4: Implement table and handling data validation
tsvSelectQuery :: forall handlingDataDescripion t db table name columns engine partitionBy orderBy .
  ( HasChSchema (Unwraped handlingDataDescripion)
  , t ~ InDatabase db (table name columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  , ToConditionalExpression handlingDataDescripion
  ) => Text
tsvSelectQuery =
  let columnsMapping = T.intercalate "," . map fst $ getSchema (Proxy @(Unwraped handlingDataDescripion))
      whereConditions = T.toStrict (T.toLazyText $ toConditionalExpression @handlingDataDescripion)
  in 
    "SELECT " <> 
    columnsMapping <> 
    " FROM " <> 
    (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name) <> 
    " " <> (if whereConditions=="" then "" else "WHERE " <> whereConditions)  <>
    " FORMAT TSV"
{-# INLINE tsvSelectQuery #-}


-- ToDo3: implement interface the same way as httpStreamChSelect 
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

  if statusCode (responseStatus resp) == 200
    then pure resp
    else throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp


-- ToDo4: Implement table and handling data validation
tsvInsertQueryHeader :: forall chSchema t db table name columns engine partitionBy orderBy .
  ( HasChSchema chSchema
  , t ~ InDatabase db (table name columns engine partitionBy orderBy)
  , KnownSymbol db
  , KnownSymbol name
  ) => Text
tsvInsertQueryHeader =
  let columnsMapping = T.intercalate "," . map fst $ getSchema (Proxy @chSchema)
  in 
    "INSERT INTO " <> 
    (T.pack . symbolVal) (Proxy @db) <> "." <> (T.pack . symbolVal) (Proxy @name) <> 
    " (" <> columnsMapping <> ")" <> 
    "FORMAT TSV\n"
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

instance HasChSchema schemaData
  => IsBuffer TBQueue schemaData
  where
  createSizedBuffer   (BufferSize size) = newTBQueueIO size
  writeToSizedBuffer  buffer d          = atomically $ writeTBQueue buffer d
  readFromSizedBuffer buffer            = atomically $ flushTBQueue buffer




data ChCredential = ChCredential
  { chLogin :: !Text
  , chPass  :: !Text
  , chUrl   :: !Text
  }
  deriving (Generic, NFData, Show, Eq)

data HttpChClient = HttpChClient H.Manager H.Request

class ChClient backend connectionManager | backend -> connectionManager where
  initClient :: ChCredential -> Maybe connectionManager -> IO backend

instance ChClient HttpChClient H.Manager where
  initClient (ChCredential login pass url) mManager = do
    man <- maybe
      (H.newManager H.defaultManagerSettings)
      pure
      mManager
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
