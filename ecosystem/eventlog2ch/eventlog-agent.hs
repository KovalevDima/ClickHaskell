{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import ClickHaskell hiding (Buffer (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (TBQueue, atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Exception (SomeException, bracketOnError, catch, finally)
import Control.Monad (forever, void)
import Data.ByteString as BS (ByteString, length)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import GHC.RTS.Events (Event (..), EventInfo (..), Header)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEvents, decodeHeader)
import Network.Socket
import Network.Socket.ByteString (recv)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

main :: IO ()
main = do
  lookupEnv "EVENTLOG_SOCKET_PATH"
    >>= \case
    Nothing -> pure ()
    Just socketPath -> do
      conn <- initChConnection
      queue <- newTBQueueIO @EventRep 1_000_000
      time <- getCurrentTime
      runConcurrently
        $ pure ()
        *> runClickHouseWriter conn queue
        *> chEventlogWrite socketPath (atomically . writeTBQueue queue . eventToRep time)


-- * Network

chEventlogWrite :: FilePath ->  (Event -> IO ()) -> Concurrently ()
chEventlogWrite socketPath writeEvent = Concurrently $ do
  sock <-
    maybe (error "Socket connection timeout") pure
      =<< timeout 3_000_000 (
        bracketOnError
          (socket AF_UNIX Stream 0)
          (\sock ->
            catch @SomeException
              (finally (shutdown sock ShutdownBoth) (close sock))
              (const $ pure ())
          )
          (\sock -> do
            connect sock (SockAddrUnix socketPath)
            pure sock
          )
        )
  streamFromSocket writeEvent sock


streamFromSocket :: (Event -> IO ()) -> Socket -> IO ()
streamFromSocket writeEvent sock = do
  buffer <- initBuffer sock
  header <- readHeader buffer decodeHeader
  (void . forever) (processEvents buffer writeEvent (decodeEvents header))

processEvents :: Buffer -> (a -> IO ()) -> Decoder a -> IO ()
processEvents buff writeEvent decoder = case decoder of
  Produce res dec -> do
    writeEvent res
    processEvents buff writeEvent dec
  Consume dec -> do
    buffValue <- readBuffer buff
    processEvents buff writeEvent (dec buffValue)
  Error _bs err -> error err
  Done _ -> error "Unexpected done"


readHeader :: Buffer -> Decoder Header -> IO Header
readHeader buff decoder = case decoder of
  Consume dec -> readHeader buff . dec =<< readBuffer buff
  Produce res (Done left) -> writeToBuffer buff left *> pure res
  Error _bs err -> error err
  Produce _res _dec -> error "Unexpected extra result in header decoder"
  Done _ -> error "Unexpected done in header decoder"


data Buffer = MkBuffer {bufferSocket :: Socket, buff :: IORef ByteString}

initBuffer :: Socket -> IO Buffer
initBuffer sock = MkBuffer sock <$> newIORef ""

writeToBuffer :: Buffer -> BS.ByteString -> IO ()
writeToBuffer MkBuffer{..} val = void (atomicModifyIORef buff (val,))

readBuffer :: Buffer -> IO BS.ByteString
readBuffer MkBuffer{..} =
  readIORef buff
    >>= (\currentBuffer ->
      case BS.length currentBuffer of
        0 -> recv bufferSocket 4096
        _ -> atomicWriteIORef buff "" *> pure currentBuffer
    )




-- * ClickHouse

runClickHouseWriter :: Connection -> TBQueue EventRep -> Concurrently ()
runClickHouseWriter conn queue =
  Concurrently $
    void . forever $ do
      insert (intoTable @"haskell_eventlog" @EventLogColumns @EventRep) conn
        . fromRecords @EventLogColumns
        =<< atomically (flushTBQueue queue)
      threadDelay 1_000_000

initChConnection :: IO Connection
initChConnection = do
  host <- lookupEnv "CLICKHOUSE_HOST"
  db   <- lookupEnv "CLICKHOUSE_DB"
  user <- lookupEnv "CLICKHOUSE_USER"
  pass <- lookupEnv "CLICKHOUSE_PASS"
  conn <-
    openConnection
      . maybe id setPort host
      . maybe id setUser user
      . maybe id setPassword pass
      . maybe id setDatabase db
      $ defaultConnectionArgs
  createEventLogTable conn
  pure conn

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
    \    `startTime` DateTime, \
    \    `evTime` DateTime64(9), \
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
