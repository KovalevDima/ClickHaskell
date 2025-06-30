{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import ClickHaskell hiding (Buffer(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (TBQueue, atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Exception (SomeException, bracketOnError, catch, finally)
import Control.Monad (forever, void)
import Data.ByteString as BS (ByteString, length)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
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
      write conn =<< atomically (flushTBQueue queue)
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
  { startTime :: UTCTime
  , evTime :: UInt64
  , evType :: ByteString
  }
  deriving (Generic, ClickHaskell EventLogColumns)

write :: Connection -> [EventRep] -> IO ()
write = insertInto @EventLogTable @EventRep

type EventLogTable = Table "haskell_eventlog" EventLogColumns
type EventLogColumns =
 '[ Column "startTime" (DateTime "")
  , Column "evTime"    (DateTime64 9 "")
  , Column "evType"    (ChString)
  ]

createEventLogTable :: Connection -> IO ()
createEventLogTable conn = command conn
    "CREATE TABLE IF NOT EXISTS haskell_eventlog \
    \( \
    \    `startTime` DateTime, \
    \    `evTime` DateTime64(9), \
    \    `evType` LowCardinality(String) \
    \) \
    \ENGINE = MergeTree \
    \PARTITION BY evType \
    \ORDER BY evType \
    \SETTINGS index_granularity = 8192;"


eventToRep :: UTCTime -> Event -> EventRep
eventToRep startTime Event{evTime, evSpec} = case evSpec of
  EventBlock{}                -> let evType = "EventBlock" in MkEventRep{..}
  UnknownEvent{}              -> let evType = "UnknownEvent" in MkEventRep{..}
  Startup{}                   -> let evType = "Startup" in MkEventRep{..}
  Shutdown{}                  -> let evType = "Shutdown" in MkEventRep{..}
  CreateThread{}              -> let evType = "CreateThread" in MkEventRep{..}
  RunThread{}                 -> let evType = "RunThread" in MkEventRep{..}
  StopThread{}                -> let evType = "StopThread" in MkEventRep{..}
  ThreadRunnable{}            -> let evType = "ThreadRunnable" in MkEventRep{..}
  MigrateThread{}             -> let evType = "MigrateThread" in MkEventRep{..}
  WakeupThread{}              -> let evType = "WakeupThread" in MkEventRep{..}
  ThreadLabel{}               -> let evType = "ThreadLabel" in MkEventRep{..}
  CreateSparkThread{}         -> let evType = "CreateSparkThread" in MkEventRep{..}
  SparkCounters{}             -> let evType = "SparkCounters" in MkEventRep{..}
  SparkCreate{}               -> let evType = "SparkCreate" in MkEventRep{..}
  SparkDud{}                  -> let evType = "SparkDud" in MkEventRep{..}
  SparkOverflow{}             -> let evType = "SparkOverflow" in MkEventRep{..}
  SparkRun{}                  -> let evType = "SparkRun" in MkEventRep{..}
  SparkSteal{}                -> let evType = "SparkSteal" in MkEventRep{..}
  SparkFizzle{}               -> let evType = "SparkFizzle" in MkEventRep{..}
  SparkGC{}                   -> let evType = "SparkGC" in MkEventRep{..}
  TaskCreate{}                -> let evType = "TaskCreate" in MkEventRep{..}
  TaskMigrate{}               -> let evType = "TaskMigrate" in MkEventRep{..}
  TaskDelete{}                -> let evType = "TaskDelete" in MkEventRep{..}
  RequestSeqGC{}              -> let evType = "RequestSeqGC" in MkEventRep{..}
  RequestParGC{}              -> let evType = "RequestParGC" in MkEventRep{..}
  StartGC{}                   -> let evType = "StartGC" in MkEventRep{..}
  GCWork{}                    -> let evType = "GCWork" in MkEventRep{..}
  GCIdle{}                    -> let evType = "GCIdle" in MkEventRep{..}
  GCDone{}                    -> let evType = "GCDone" in MkEventRep{..}
  EndGC{}                     -> let evType = "EndGC" in MkEventRep{..}
  GlobalSyncGC{}              -> let evType = "GlobalSyncGC" in MkEventRep{..}
  GCStatsGHC{}                -> let evType = "GCStatsGHC" in MkEventRep{..}
  MemReturn{}                 -> let evType = "MemReturn" in MkEventRep{..}
  HeapAllocated{}             -> let evType = "HeapAllocated" in MkEventRep{..}
  HeapSize{}                  -> let evType = "HeapSize" in MkEventRep{..}
  BlocksSize{}                -> let evType = "BlocksSize" in MkEventRep{..}
  HeapLive{}                  -> let evType = "HeapLive" in MkEventRep{..}
  HeapInfoGHC{}               -> let evType = "HeapInfoGHC" in MkEventRep{..}
  CapCreate{}                 -> let evType = "CapCreate" in MkEventRep{..}
  CapDelete{}                 -> let evType = "CapDelete" in MkEventRep{..}
  CapDisable{}                -> let evType = "CapDisable" in MkEventRep{..}
  CapEnable{}                 -> let evType = "CapEnable" in MkEventRep{..}
  CapsetCreate{}              -> let evType = "CapsetCreate" in MkEventRep{..}
  CapsetDelete{}              -> let evType = "CapsetDelete" in MkEventRep{..}
  CapsetAssignCap{}           -> let evType = "CapsetAssignCap" in MkEventRep{..}
  CapsetRemoveCap{}           -> let evType = "CapsetRemoveCap" in MkEventRep{..}
  RtsIdentifier{}             -> let evType = "RtsIdentifier" in MkEventRep{..}
  ProgramArgs{}               -> let evType = "ProgramArgs" in MkEventRep{..}
  ProgramEnv{}                -> let evType = "ProgramEnv" in MkEventRep{..}
  OsProcessPid{}              -> let evType = "OsProcessPid" in MkEventRep{..}
  OsProcessParentPid{}        -> let evType = "OsProcessParentPid" in MkEventRep{..}
  WallClockTime{}             -> let evType = "WallClockTime" in MkEventRep{..}
  Message{}                   -> let evType = "Message" in MkEventRep{..}
  UserMessage{}               -> let evType = "UserMessage" in MkEventRep{..}
  UserMarker{}                -> let evType = "UserMarker" in MkEventRep{..}
  Version{}                   -> let evType = "Version" in MkEventRep{..}
  ProgramInvocation{}         -> let evType = "ProgramInvocation" in MkEventRep{..}
  CreateMachine{}             -> let evType = "CreateMachine" in MkEventRep{..}
  KillMachine{}               -> let evType = "KillMachine" in MkEventRep{..}
  CreateProcess{}             -> let evType = "CreateProcess" in MkEventRep{..}
  KillProcess{}               -> let evType = "KillProcess" in MkEventRep{..}
  AssignThreadToProcess{}     -> let evType = "AssignThreadToProcess" in MkEventRep{..}
  EdenStartReceive{}          -> let evType = "EdenStartReceive" in MkEventRep{..}
  EdenEndReceive{}            -> let evType = "EdenEndReceive" in MkEventRep{..}
  SendMessage{}               -> let evType = "SendMessage" in MkEventRep{..}
  ReceiveMessage{}            -> let evType = "ReceiveMessage" in MkEventRep{..}
  SendReceiveLocalMessage{}   -> let evType = "SendReceiveLocalMessage" in MkEventRep{..}
  InternString{}              -> let evType = "InternString" in MkEventRep{..}
  MerStartParConjunction{}    -> let evType = "MerStartParConjunction" in MkEventRep{..}
  MerEndParConjunction{}      -> let evType = "MerEndParConjunction" in MkEventRep{..}
  MerEndParConjunct{}         -> let evType = "MerEndParConjunct" in MkEventRep{..}
  MerCreateSpark{}            -> let evType = "MerCreateSpark" in MkEventRep{..}
  MerFutureCreate{}           -> let evType = "MerFutureCreate" in MkEventRep{..}
  MerFutureWaitNosuspend{}    -> let evType = "MerFutureWaitNosuspend" in MkEventRep{..}
  MerFutureWaitSuspended{}    -> let evType = "MerFutureWaitSuspended" in MkEventRep{..}
  MerFutureSignal{}           -> let evType = "MerFutureSignal" in MkEventRep{..}
  MerLookingForGlobalThread{} -> let evType = "MerLookingForGlobalThread" in MkEventRep{..}
  MerWorkStealing{}           -> let evType = "MerWorkStealing" in MkEventRep{..}
  MerLookingForLocalSpark{}   -> let evType = "MerLookingForLocalSpark" in MkEventRep{..}
  MerReleaseThread{}          -> let evType = "MerReleaseThread" in MkEventRep{..}
  MerCapSleeping{}            -> let evType = "MerCapSleeping" in MkEventRep{..}
  MerCallingMain{}            -> let evType = "MerCallingMain" in MkEventRep{..}
  PerfName{}                  -> let evType = "PerfName" in MkEventRep{..}
  PerfCounter{}               -> let evType = "PerfCounter" in MkEventRep{..}
  PerfTracepoint{}            -> let evType = "PerfTracepoint" in MkEventRep{..}
  HeapProfBegin{}             -> let evType = "HeapProfBegin" in MkEventRep{..}
  HeapProfCostCentre{}        -> let evType = "HeapProfCostCentre" in MkEventRep{..}
  InfoTableProv{}             -> let evType = "InfoTableProv" in MkEventRep{..}
  HeapProfSampleBegin{}       -> let evType = "HeapProfSampleBegin" in MkEventRep{..}
  HeapProfSampleEnd{}         -> let evType = "HeapProfSampleEnd" in MkEventRep{..}
  HeapBioProfSampleBegin{}    -> let evType = "HeapBioProfSampleBegin" in MkEventRep{..}
  HeapProfSampleCostCentre{}  -> let evType = "HeapProfSampleCostCentre" in MkEventRep{..}
  HeapProfSampleString{}      -> let evType = "HeapProfSampleString" in MkEventRep{..}
  ProfSampleCostCentre{}      -> let evType = "ProfSampleCostCentre" in MkEventRep{..}
  ProfBegin{}                 -> let evType = "ProfBegin" in MkEventRep{..}
  UserBinaryMessage{}         -> let evType = "UserBinaryMessage" in MkEventRep{..}
  ConcMarkBegin{}             -> let evType = "ConcMarkBegin" in MkEventRep{..}
  ConcMarkEnd{}               -> let evType = "ConcMarkEnd" in MkEventRep{..}
  ConcSyncBegin{}             -> let evType = "ConcSyncBegin" in MkEventRep{..}
  ConcSyncEnd{}               -> let evType = "ConcSyncEnd" in MkEventRep{..}
  ConcSweepBegin{}            -> let evType = "ConcSweepBegin" in MkEventRep{..}
  ConcSweepEnd{}              -> let evType = "ConcSweepEnd" in MkEventRep{..}
  ConcUpdRemSetFlush{}        -> let evType = "ConcUpdRemSetFlush" in MkEventRep{..}
  NonmovingHeapCensus{}       -> let evType = "NonmovingHeapCensus" in MkEventRep{..}
  NonmovingPrunedSegments{}   -> let evType = "NonmovingPrunedSegments" in MkEventRep{..}
  TickyCounterDef{}           -> let evType = "TickyCounterDef" in MkEventRep{..}
  TickyCounterSample{}        -> let evType = "TickyCounterSample" in MkEventRep{..}
  TickyBeginSample{}          -> let evType = "TickyBeginSample" in MkEventRep{..}
