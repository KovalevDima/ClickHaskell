{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module EventLogAgent.ClickHouse where

import ClickHaskell
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Monad (forever, void)
import Data.ByteString as BS (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.RTS.Events (Event (..), EventInfo (..))

-- * ClickHouse

data ClickHouseWriterArgs = MkClickHouseWriterArgs
  { clickHouseHost :: Maybe String
  , clickHouseUser :: Maybe String
  , clickHousePass :: Maybe String
  , clickHouseDb   :: Maybe String
  , connInitTime   :: UTCTime
  , produceEvents  :: IO [Event]
  }

data ClickHouseWriter = MkClickHouseWriter
  { clickHouseWriter :: Concurrently ()
  }

initClickHouseWriter :: ClickHouseWriterArgs ->  IO ClickHouseWriter
initClickHouseWriter MkClickHouseWriterArgs{..}  = do
  conn <-
    openConnection
      . maybe id setPort clickHouseHost
      . maybe id setUser clickHouseUser
      . maybe id setPassword clickHousePass
      . maybe id setDatabase clickHouseDb
      $ defaultConnectionArgs

  createEventLogTable conn

  let
    clickHouseWriter =
      Concurrently $
        void . forever $ do
          insert insertQ conn . map (eventToRep connInitTime) =<< produceEvents
          threadDelay 1_000_000
  pure $ MkClickHouseWriter{..}

insertQ :: Insert EventLogColumns EventRep
insertQ = intoTable @"haskell_eventlog" @EventLogColumns @EventRep

data EventRep = MkEventRep
  { startTime      :: UTCTime
  , evTime         :: UInt64
  , evType         :: ByteString
  , cap            :: Int64
  , gcGen          :: Int64
  , gcCopied       :: UInt64
  , heapCapset     :: UInt32
  , heapAllocBytes :: UInt64
  , heapSizeBytes  :: UInt64
  , heapBlocksSize :: UInt64
  , message        :: ByteString
  }
  deriving (Generic, ClickHaskell EventLogColumns)

type EventLogColumns =
 '[ Column "startTime"      (DateTime "")
  , Column "evTime"         (DateTime64 9 "")
  , Column "evType"         (ChString)
  , Column "cap"            (Int64)
  , Column "gcGen"          (Int64)
  , Column "gcCopied"       (UInt64)
  , Column "heapCapset"     (UInt32)
  , Column "heapAllocBytes" (UInt64)
  , Column "heapSizeBytes"  (UInt64)
  , Column "heapBlocksSize" (UInt64)
  , Column "message"        (ChString)
  ]

createEventLogTable :: Connection -> IO ()
createEventLogTable conn =
  command conn
    "CREATE TABLE IF NOT EXISTS haskell_eventlog \
    \( \
    \    `startTime` DateTime CODEC(Delta, ZSTD), \
    \    `evTime` DateTime64(9) CODEC(Delta, ZSTD), \
    \    `evType` LowCardinality(String), \
    \    `cap` Int64, \
    \    `gcGen` Int64, \
    \    `gcCopied` UInt64, \
    \    `heapCapset` UInt32, \
    \    `heapAllocBytes` UInt64, \
    \    `heapSizeBytes` UInt64, \
    \    `heapBlocksSize` UInt64, \
    \    `message` String \
    \) \
    \ENGINE = MergeTree \
    \PARTITION BY evType \
    \ORDER BY evType \
    \SETTINGS index_granularity = 8192;"

eventToRep :: UTCTime -> Event -> EventRep
eventToRep startTime Event{evTime, evSpec, evCap} =
  let
    evType = eventToName evSpec
    cap    = maybe (-1) fromIntegral evCap
    gcGen    = event2gcGen evSpec
    gcCopied = event2gcCopied evSpec
    heapCapset     = event2gcHeapCapset evSpec
    heapAllocBytes = event2heapAllocBytes evSpec
    heapSizeBytes  = event2heapSizeBytes evSpec
    heapBlocksSize = event2blocksSize evSpec
    message        = event2message evSpec
  in MkEventRep{..}




-- * Events extractors

event2gcHeapCapset :: EventInfo -> UInt32
event2gcHeapCapset evSpec = case evSpec of
  GCStatsGHC{heapCapset} -> heapCapset
  MemReturn{heapCapset} -> heapCapset
  HeapAllocated{heapCapset} -> heapCapset
  HeapSize{heapCapset} -> heapCapset
  BlocksSize{heapCapset} -> heapCapset
  HeapLive{heapCapset} -> heapCapset
  HeapInfoGHC{heapCapset} -> heapCapset
  _ -> 0

event2message :: EventInfo -> ByteString
event2message = \case
  UserBinaryMessage{payload} -> payload
  UserMessage{msg} -> encodeUtf8 msg
  Message{msg} -> encodeUtf8 msg
  _ -> ""

event2gcGen :: EventInfo -> Int64
event2gcGen = \case GCStatsGHC{gen} -> fromIntegral gen; _ -> (-1)

event2gcCopied :: EventInfo -> UInt64
event2gcCopied = \case GCStatsGHC{copied} -> copied; _ -> 0

event2heapAllocBytes :: EventInfo -> UInt64
event2heapAllocBytes = \case HeapAllocated{allocBytes} -> allocBytes; _ -> 0

event2blocksSize :: EventInfo -> UInt64
event2blocksSize = \case BlocksSize{blocksSize} -> blocksSize; _ -> 0

event2heapSizeBytes :: EventInfo -> UInt64
event2heapSizeBytes = \case HeapSize{sizeBytes} -> sizeBytes; _ -> 0

eventToName :: EventInfo -> ByteString 
eventToName evSpec = case evSpec of
  EventBlock{}                -> "EventBlock"
  UnknownEvent{}              -> "UnknownEvent"
  Startup{}                   -> "Startup"
  Shutdown{}                  -> "Shutdown"
  CreateThread{}              -> "CreateThread"
  RunThread{}                 -> "RunThread"
  StopThread{}                -> "StopThread"
  ThreadRunnable{}            -> "ThreadRunnable"
  MigrateThread{}             -> "MigrateThread"
  WakeupThread{}              -> "WakeupThread"
  ThreadLabel{}               -> "ThreadLabel"
  CreateSparkThread{}         -> "CreateSparkThread"
  SparkCounters{}             -> "SparkCounters"
  SparkCreate{}               -> "SparkCreate"
  SparkDud{}                  -> "SparkDud"
  SparkOverflow{}             -> "SparkOverflow"
  SparkRun{}                  -> "SparkRun"
  SparkSteal{}                -> "SparkSteal"
  SparkFizzle{}               -> "SparkFizzle"
  SparkGC{}                   -> "SparkGC"
  TaskCreate{}                -> "TaskCreate"
  TaskMigrate{}               -> "TaskMigrate"
  TaskDelete{}                -> "TaskDelete"
  RequestSeqGC{}              -> "RequestSeqGC"
  RequestParGC{}              -> "RequestParGC"
  StartGC{}                   -> "StartGC"
  GCWork{}                    -> "GCWork"
  GCIdle{}                    -> "GCIdle"
  GCDone{}                    -> "GCDone"
  EndGC{}                     -> "EndGC"
  GlobalSyncGC{}              -> "GlobalSyncGC"
  GCStatsGHC{}                -> "GCStatsGHC"
  MemReturn{}                 -> "MemReturn"
  HeapAllocated{}             -> "HeapAllocated"
  HeapSize{}                  -> "HeapSize"
  BlocksSize{}                -> "BlocksSize"
  HeapLive{}                  -> "HeapLive"
  HeapInfoGHC{}               -> "HeapInfoGHC"
  CapCreate{}                 -> "CapCreate"
  CapDelete{}                 -> "CapDelete"
  CapDisable{}                -> "CapDisable"
  CapEnable{}                 -> "CapEnable"
  CapsetCreate{}              -> "CapsetCreate"
  CapsetDelete{}              -> "CapsetDelete"
  CapsetAssignCap{}           -> "CapsetAssignCap"
  CapsetRemoveCap{}           -> "CapsetRemoveCap"
  RtsIdentifier{}             -> "RtsIdentifier"
  ProgramArgs{}               -> "ProgramArgs"
  ProgramEnv{}                -> "ProgramEnv"
  OsProcessPid{}              -> "OsProcessPid"
  OsProcessParentPid{}        -> "OsProcessParentPid"
  WallClockTime{}             -> "WallClockTime"
  Message{}                   -> "Message"
  UserMessage{}               -> "UserMessage"
  UserMarker{}                -> "UserMarker"
  Version{}                   -> "Version"
  ProgramInvocation{}         -> "ProgramInvocation"
  CreateMachine{}             -> "CreateMachine"
  KillMachine{}               -> "KillMachine"
  CreateProcess{}             -> "CreateProcess"
  KillProcess{}               -> "KillProcess"
  AssignThreadToProcess{}     -> "AssignThreadToProcess"
  EdenStartReceive{}          -> "EdenStartReceive"
  EdenEndReceive{}            -> "EdenEndReceive"
  SendMessage{}               -> "SendMessage"
  ReceiveMessage{}            -> "ReceiveMessage"
  SendReceiveLocalMessage{}   -> "SendReceiveLocalMessage"
  InternString{}              -> "InternString"
  MerStartParConjunction{}    -> "MerStartParConjunction"
  MerEndParConjunction{}      -> "MerEndParConjunction"
  MerEndParConjunct{}         -> "MerEndParConjunct"
  MerCreateSpark{}            -> "MerCreateSpark"
  MerFutureCreate{}           -> "MerFutureCreate"
  MerFutureWaitNosuspend{}    -> "MerFutureWaitNosuspend"
  MerFutureWaitSuspended{}    -> "MerFutureWaitSuspended"
  MerFutureSignal{}           -> "MerFutureSignal"
  MerLookingForGlobalThread{} -> "MerLookingForGlobalThread"
  MerWorkStealing{}           -> "MerWorkStealing"
  MerLookingForLocalSpark{}   -> "MerLookingForLocalSpark"
  MerReleaseThread{}          -> "MerReleaseThread"
  MerCapSleeping{}            -> "MerCapSleeping"
  MerCallingMain{}            -> "MerCallingMain"
  PerfName{}                  -> "PerfName"
  PerfCounter{}               -> "PerfCounter"
  PerfTracepoint{}            -> "PerfTracepoint"
  HeapProfBegin{}             -> "HeapProfBegin"
  HeapProfCostCentre{}        -> "HeapProfCostCentre"
  InfoTableProv{}             -> "InfoTableProv"
  HeapProfSampleBegin{}       -> "HeapProfSampleBegin"
  HeapProfSampleEnd{}         -> "HeapProfSampleEnd"
  HeapBioProfSampleBegin{}    -> "HeapBioProfSampleBegin"
  HeapProfSampleCostCentre{}  -> "HeapProfSampleCostCentre"
  HeapProfSampleString{}      -> "HeapProfSampleString"
  ProfSampleCostCentre{}      -> "ProfSampleCostCentre"
  ProfBegin{}                 -> "ProfBegin"
  UserBinaryMessage{}         -> "UserBinaryMessage"
  ConcMarkBegin{}             -> "ConcMarkBegin"
  ConcMarkEnd{}               -> "ConcMarkEnd"
  ConcSyncBegin{}             -> "ConcSyncBegin"
  ConcSyncEnd{}               -> "ConcSyncEnd"
  ConcSweepBegin{}            -> "ConcSweepBegin"
  ConcSweepEnd{}              -> "ConcSweepEnd"
  ConcUpdRemSetFlush{}        -> "ConcUpdRemSetFlush"
  NonmovingHeapCensus{}       -> "NonmovingHeapCensus"
  NonmovingPrunedSegments{}   -> "NonmovingPrunedSegments"
  TickyCounterDef{}           -> "TickyCounterDef"
  TickyCounterSample{}        -> "TickyCounterSample"
  TickyBeginSample{}          -> "TickyBeginSample"
