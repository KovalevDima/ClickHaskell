{-# LANGUAGE
    AllowAmbiguousTypes
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
#-}

module ClickHaskell.Buffering where


-- GHC included
import Control.Concurrent     (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM (TBQueue, atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Exception      (SomeException, handle)
import Control.Monad          (forever, unless)
import GHC.Num                (Natural)


-- | Forks buffer flusher with given frequency 
--
forkBufferFlusher :: (IsBuffer buffer schemaData)
  => Natural                  -- ^ Flushes frequency
  -> buffer schemaData        -- ^ Buffer with schema specialized data
  -> (SomeException -> IO ()) -- ^ Flush action exception handler
  -> ([schemaData] -> IO ())  -- ^ Flush action
  -> IO ThreadId
forkBufferFlusher freq buffer exceptionHandler flushAction
  = forkIO . forever
  $ do
  threadDelay (fromIntegral freq)
  bufferData <- readFromSizedBuffer buffer
  unless (null bufferData)
    ( handle exceptionHandler (flushAction bufferData)
    )
{-# NOINLINE forkBufferFlusher #-}


class IsBuffer buffer schemaData
  where
  writeToSizedBuffer  :: buffer schemaData -> schemaData -> IO ()
  createSizedBuffer   :: Natural -> IO (buffer schemaData)
  readFromSizedBuffer :: buffer schemaData  -> IO [schemaData]


type DefaultBuffer = TBQueue

instance IsBuffer DefaultBuffer schemaData
  where
  createSizedBuffer            = newTBQueueIO
  writeToSizedBuffer  buffer d = atomically $ writeTBQueue buffer d
  readFromSizedBuffer buffer   = atomically $ flushTBQueue buffer
  {-# INLINE createSizedBuffer #-}
  {-# INLINE writeToSizedBuffer #-}
  {-# INLINE readFromSizedBuffer #-}
