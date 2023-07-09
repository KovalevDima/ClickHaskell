{-# LANGUAGE
  AllowAmbiguousTypes,
  DerivingStrategies,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses
#-}

module ClickHaskell.Buffering where

-- Internal dependencies
import ClickHaskell.DataDsl.Inserting (InsertableInto)

-- GHC included libraries imports
import Control.Concurrent     (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM (TBQueue, atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Exception      (SomeException, handle)
import Control.Monad          (forever, unless)
import GHC.Num                (Natural)


-- | Forks buffer flusher with given frequency 
--
forkBufferFlusher :: (InsertableInto table schemaData, IsBuffer buffer schemaData)
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

instance IsBuffer DefaultBuffer schemaData
  where
  createSizedBuffer   (BufferSize size) = newTBQueueIO size
  writeToSizedBuffer  buffer d          = atomically $ writeTBQueue buffer d
  {-# NOINLINE writeToSizedBuffer #-}
  readFromSizedBuffer buffer            = atomically $ flushTBQueue buffer
  {-# NOINLINE readFromSizedBuffer #-}
