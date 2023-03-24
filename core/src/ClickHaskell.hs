{-# LANGUAGE
    DataKinds
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
  -- * Insert DSL 
  httpStreamChInsert, Database(..), Table(..)

  -- * Buffered writing abstractions
  , writeToSizedBuffer, createSizedBuffer, readFromSizedBuffer, forkBufferFlusher, BufferSize(..), TBQueue

  -- * Client abstraction
  , ChClient(initClient), HttpChClient, ChCredential(ChCredential, chLogin, chPass, chUrl)
  ) where

-- GHC boot packages
import Data.ByteString         as BS (ByteString, toStrict)
import Data.ByteString.Lazy    as BSL (ByteString)
import Data.Data               (Proxy(..))
import Data.Text               as T (Text, intercalate, unpack)
import Data.Text.Encoding      as T (encodeUtf8, decodeUtf8)
import Control.Concurrent      (forkIO, ThreadId, threadDelay)
import Control.Concurrent.STM  (TBQueue, writeTBQueue, atomically, newTBQueueIO, flushTBQueue)
import Control.DeepSeq         (NFData)
import Control.Exception       (SomeException, handle, Exception, throw)
import Control.Monad           (forever, unless)
import GHC.Exts                (IsString)
import GHC.Generics            (Generic)
import GHC.Num                 (Natural)

-- External packages
import Conduit                     (yieldMany, yield, ConduitM, MonadIO)
import Network.HTTP.Client         as H (newManager, Manager, Response, httpLbs, responseStatus, responseBody)
import Network.HTTP.Client.Conduit as H (Request (..), defaultManagerSettings, parseRequest, requestBodySourceChunked)
import Network.HTTP.Simple         as H (setRequestManager)
import Network.HTTP.Types          (statusCode)


-- Internal packages
import ClickHaskell.TableDsl (HasChSchema (getSchema), toBs, ChSchema (..))


newtype Database = Database Text deriving newtype (Show, IsString)
newtype Table    = Table    Text deriving newtype (Show, IsString)

httpStreamChInsert :: (HasChSchema chSchema)
  => HttpChClient -> [chSchema] -> Database -> Table -> IO (H.Response BSL.ByteString)
httpStreamChInsert (HttpChClient man req) schemaList db table
  = do
    resp <- H.httpLbs req{requestBody = H.requestBodySourceChunked $ mkChInsertStreamBody schemaList db table} man
    if statusCode (responseStatus resp) == 200
      then pure resp
      else throw $ ChException $ T.decodeUtf8 $ BS.toStrict $ responseBody resp

mkChInsertStreamBody :: (HasChSchema chSchema, MonadIO m)
  => [chSchema] -> Database -> Table -> ConduitM () BS.ByteString m ()
mkChInsertStreamBody (schemaList :: [chSchema]) db table
  =  yield     (encodeUtf8 $ tsvInsertHeader (Proxy :: Proxy chSchema) db table)
  >> yieldMany (map toBs schemaList)
{-# NOINLINE mkChInsertStreamBody #-}

tsvInsertHeader :: (HasChSchema schema) => Proxy schema -> Database -> Table -> Text
tsvInsertHeader schemaRep (Database db) (Table table)
  = "INSERT INTO " <> db <> "." <> table <> " (" <> columnsMapping <> ") FORMAT TSV\n"
  where columnsMapping = T.intercalate "," . map fst . schema $ getSchema schemaRep


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

class ChClient backend connectionManager | backend -> connectionManager where
  initClient :: Maybe connectionManager -> ChCredential -> IO backend

instance ChClient HttpChClient H.Manager where
  initClient mManager (ChCredential login pass url) = do
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
