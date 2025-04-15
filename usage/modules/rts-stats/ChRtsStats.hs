{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module ChRtsStats where

import ClickHaskell
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Monad (forever)
import GHC.Generics (Generic)
import GHC.Stats

initPerformanceTracker :: ChCredential -> IO (Concurrently())
initPerformanceTracker cred = do
  _perfChConnection <- openNativeConnection cred
  performanceQueue <- newTBQueueIO 100

  pure $ 
    Concurrently (forever $ do
      atomically . writeTBQueue performanceQueue . (\RTSStats{..} -> MkPerformanceStat{..})
        =<< getRTSStats
      threadDelay 1_000_000
    )
    *>
    Concurrently (forever $ do
      _ <- atomically (flushTBQueue performanceQueue)
      threadDelay 10_000_000
    )

data PerformanceStat = MkPerformanceStat
  { max_live_bytes :: UInt64
  }
  deriving (Generic)
