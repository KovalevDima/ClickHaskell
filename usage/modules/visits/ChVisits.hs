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

module ChVisits where

import ClickHaskell
  ( ChString, DateTime
  , UInt16, UInt32
  , ClickHaskell, insertInto
  , Connection, openConnection, defaultConnectionArgs
  , Column, Table
  , View, selectFromView, Parameter, parameter
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (TBQueue, TChan, TVar, atomically, flushTBQueue, newTVarIO, writeTChan, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Data.Aeson (ToJSON)
import Data.ByteString (StrictByteString)
import Data.Time (UTCTime)
import Data.Word (Word32)
import GHC.Generics (Generic)


data DocsStatisticsArgs = MkDocsStatisticsArgs
  { docsStatQueue  :: TBQueue DocsStatistics
  , broadcastChan  :: TChan HistoryData
  }

initVisitsTracker :: DocsStatisticsArgs -> IO (Concurrently (), TVar HistoryData)
initVisitsTracker MkDocsStatisticsArgs{..} = do
  clickHouse     <- openConnection defaultConnectionArgs
  currentHistory <- newTVarIO . History =<< readCurrentHistoryLast clickHouse 24

  pure
    ( Concurrently (forever $ do
        catch (do
            dataToWrite <- (atomically . flushTBQueue) docsStatQueue
            insertInto @DocsStatTable clickHouse dataToWrite
          )
          (print @SomeException)
        threadDelay 5_000_000
      )
      *>
      Concurrently (forever $ do
        historyByHours <- readCurrentHistoryLast clickHouse 1
        case historyByHours of
          update:_ -> atomically $ (writeTChan broadcastChan) (HistoryUpdate update)
          _        -> pure ()
        threadDelay 1_000_000
      )
      *>
      Concurrently (forever $ do
        atomically . writeTVar currentHistory . History =<< readCurrentHistoryLast clickHouse 24
        threadDelay 300_000_000
      )
    , currentHistory
    )

readCurrentHistoryLast :: Connection -> UInt16 -> IO [HourData]
readCurrentHistoryLast clickHouse hours =
  concat <$>
    selectFromView
      @HistoryByHours
      clickHouse
      (parameter @"hoursLength" @UInt16 hours)
      pure

data HistoryData = History{history :: [HourData]} | HistoryUpdate{realtime :: HourData}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- ** Writing data

data DocsStatistics = MkDocsStatistics
  { time       :: UTCTime
  , path       :: StrictByteString
  , remoteAddr :: Word32
  }
  deriving stock (Generic)
  deriving anyclass (ClickHaskell DocStatColumns)

type DocsStatTable = Table "ClickHaskellStats" DocStatColumns
type DocStatColumns =
 '[ Column "time" (DateTime "")
  , Column "path" ChString
  , Column "remoteAddr" UInt32
  ]

-- ** Reading data

data HourData = MkHourData
  { hour :: Word32
  , visits :: Word32
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ClickHaskell HistoryColumns)

type HistoryByHours =
  View
    "historyByHours"
    HistoryColumns
   '[ Parameter "hoursLength" UInt16
    ]
type HistoryColumns =
 '[ Column "hour" UInt32
  , Column "visits" UInt32
  ]

{-
<pre><code class="sql" data-lang="sql"
>CREATE TABLE default.ClickHaskellStats
(
    `time` DateTime,
    `path` LowCardinality(String),
    `remoteAddr` UInt32
)
ENGINE = MergeTree
PARTITION BY path
ORDER BY path
SETTINGS index_granularity = 8192;

CREATE OR REPLACE VIEW default.historyByHours
AS SELECT
    toUInt32(intDiv(toUInt32(time), 3600) * 3600) AS hour,
    toUInt32(countDistinct(remoteAddr)) AS visits
FROM default.ClickHaskellStats
WHERE hour > (now() - ({hoursLength:UInt16} * 3600))
GROUP BY hour
ORDER BY hour ASC;
</code></pre>
-}