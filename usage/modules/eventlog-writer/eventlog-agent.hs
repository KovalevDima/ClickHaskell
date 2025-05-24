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

import ClickHaskell
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (TBQueue, atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Exception (SomeException, bracketOnError, catch, finally)
import Control.Monad (forever, void)
import Data.ByteString as BS (ByteString, length)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import Data.Text as T (pack)
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
      . maybe id (setPort) host
      . maybe id (setUser . T.pack) user
      . maybe id (setPassword . T.pack) pass
      . maybe id (setDatabase . T.pack) db
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
eventToRep startTime Event{evTime, evSpec} =
  let mkEvent = \evType -> MkEventRep{..} in
  case evSpec of
    EventBlock{}                -> let evType = "EventBlock" in MkEventRep{..}
    UnknownEvent{}              -> mkEvent "UnknownEvent"
    Startup{}                   -> mkEvent "Startup"
    Shutdown{}                  -> mkEvent "Shutdown"
    CreateThread{}              -> mkEvent "CreateThread"
    RunThread{}                 -> mkEvent "RunThread"
    StopThread{}                -> mkEvent "StopThread"
    ThreadRunnable{}            -> mkEvent "ThreadRunnable"
    MigrateThread{}             -> mkEvent "MigrateThread"
    WakeupThread{}              -> mkEvent "WakeupThread"
    ThreadLabel{}               -> mkEvent "ThreadLabel"
    CreateSparkThread{}         -> mkEvent "CreateSparkThread"
    SparkCounters{}             -> mkEvent "SparkCounters"
    SparkCreate{}               -> mkEvent "SparkCreate"
    SparkDud{}                  -> mkEvent "SparkDud"
    SparkOverflow{}             -> mkEvent "SparkOverflow"
    SparkRun{}                  -> mkEvent "SparkRun"
    SparkSteal{}                -> mkEvent "SparkSteal"
    SparkFizzle{}               -> mkEvent "SparkFizzle"
    SparkGC{}                   -> mkEvent "SparkGC"
    TaskCreate{}                -> mkEvent "TaskCreate"
    TaskMigrate{}               -> mkEvent "TaskMigrate"
    TaskDelete{}                -> mkEvent "TaskDelete"
    RequestSeqGC{}              -> mkEvent "RequestSeqGC"
    RequestParGC{}              -> mkEvent "RequestParGC"
    StartGC{}                   -> mkEvent "StartGC"
    GCWork{}                    -> mkEvent "GCWork"
    GCIdle{}                    -> mkEvent "GCIdle"
    GCDone{}                    -> mkEvent "GCDone"
    EndGC{}                     -> mkEvent "EndGC"
    GlobalSyncGC{}              -> mkEvent "GlobalSyncGC"
    GCStatsGHC{}                -> mkEvent "GCStatsGHC"
    MemReturn{}                 -> mkEvent "MemReturn"
    HeapAllocated{}             -> mkEvent "HeapAllocated"
    HeapSize{}                  -> mkEvent "HeapSize"
    BlocksSize{}                -> mkEvent "BlocksSize"
    HeapLive{}                  -> mkEvent "HeapLive"
    HeapInfoGHC{}               -> mkEvent "HeapInfoGHC"
    CapCreate{}                 -> mkEvent "CapCreate"
    CapDelete{}                 -> mkEvent "CapDelete"
    CapDisable{}                -> mkEvent "CapDisable"
    CapEnable{}                 -> mkEvent "CapEnable"
    CapsetCreate{}              -> mkEvent "CapsetCreate"
    CapsetDelete{}              -> mkEvent "CapsetDelete"
    CapsetAssignCap{}           -> mkEvent "CapsetAssignCap"
    CapsetRemoveCap{}           -> mkEvent "CapsetRemoveCap"
    RtsIdentifier{}             -> mkEvent "RtsIdentifier"
    ProgramArgs{}               -> mkEvent "ProgramArgs"
    ProgramEnv{}                -> mkEvent "ProgramEnv"
    OsProcessPid{}              -> mkEvent "OsProcessPid"
    OsProcessParentPid{}        -> mkEvent "OsProcessParentPid"
    WallClockTime{}             -> mkEvent "WallClockTime"
    Message{}                   -> mkEvent "Message"
    UserMessage{}               -> mkEvent "UserMessage"
    UserMarker{}                -> mkEvent "UserMarker"
    Version{}                   -> mkEvent "Version"
    ProgramInvocation{}         -> mkEvent "ProgramInvocation"
    CreateMachine{}             -> mkEvent "CreateMachine"
    KillMachine{}               -> mkEvent "KillMachine"
    CreateProcess{}             -> mkEvent "CreateProcess"
    KillProcess{}               -> mkEvent "KillProcess"
    AssignThreadToProcess{}     -> mkEvent "AssignThreadToProcess"
    EdenStartReceive{}          -> mkEvent "EdenStartReceive"
    EdenEndReceive{}            -> mkEvent "EdenEndReceive"
    SendMessage{}               -> mkEvent "SendMessage"
    ReceiveMessage{}            -> mkEvent "ReceiveMessage"
    SendReceiveLocalMessage{}   -> mkEvent "SendReceiveLocalMessage"
    InternString{}              -> mkEvent "InternString"
    MerStartParConjunction{}    -> mkEvent "MerStartParConjunction"
    MerEndParConjunction{}      -> mkEvent "MerEndParConjunction"
    MerEndParConjunct{}         -> mkEvent "MerEndParConjunct"
    MerCreateSpark{}            -> mkEvent "MerCreateSpark"
    MerFutureCreate{}           -> mkEvent "MerFutureCreate"
    MerFutureWaitNosuspend{}    -> mkEvent "MerFutureWaitNosuspend"
    MerFutureWaitSuspended{}    -> mkEvent "MerFutureWaitSuspended"
    MerFutureSignal{}           -> mkEvent "MerFutureSignal"
    MerLookingForGlobalThread{} -> mkEvent "MerLookingForGlobalThread"
    MerWorkStealing{}           -> mkEvent "MerWorkStealing"
    MerLookingForLocalSpark{}   -> mkEvent "MerLookingForLocalSpark"
    MerReleaseThread{}          -> mkEvent "MerReleaseThread"
    MerCapSleeping{}            -> mkEvent "MerCapSleeping"
    MerCallingMain{}            -> mkEvent "MerCallingMain"
    PerfName{}                  -> mkEvent "PerfName"
    PerfCounter{}               -> mkEvent "PerfCounter"
    PerfTracepoint{}            -> mkEvent "PerfTracepoint"
    HeapProfBegin{}             -> mkEvent "HeapProfBegin"
    HeapProfCostCentre{}        -> mkEvent "HeapProfCostCentre"
    InfoTableProv{}             -> mkEvent "InfoTableProv"
    HeapProfSampleBegin{}       -> mkEvent "HeapProfSampleBegin"
    HeapProfSampleEnd{}         -> mkEvent "HeapProfSampleEnd"
    HeapBioProfSampleBegin{}    -> mkEvent "HeapBioProfSampleBegin"
    HeapProfSampleCostCentre{}  -> mkEvent "HeapProfSampleCostCentre"
    HeapProfSampleString{}      -> mkEvent "HeapProfSampleString"
    ProfSampleCostCentre{}      -> mkEvent "ProfSampleCostCentre"
    ProfBegin{}                 -> mkEvent "ProfBegin"
    UserBinaryMessage{}         -> mkEvent "UserBinaryMessage"
    ConcMarkBegin{}             -> mkEvent "ConcMarkBegin"
    ConcMarkEnd{}               -> mkEvent "ConcMarkEnd"
    ConcSyncBegin{}             -> mkEvent "ConcSyncBegin"
    ConcSyncEnd{}               -> mkEvent "ConcSyncEnd"
    ConcSweepBegin{}            -> mkEvent "ConcSweepBegin"
    ConcSweepEnd{}              -> mkEvent "ConcSweepEnd"
    ConcUpdRemSetFlush{}        -> mkEvent "ConcUpdRemSetFlush"
    NonmovingHeapCensus{}       -> mkEvent "NonmovingHeapCensus"
    NonmovingPrunedSegments{}   -> mkEvent "NonmovingPrunedSegments"
    TickyCounterDef{}           -> mkEvent "TickyCounterDef"
    TickyCounterSample{}        -> mkEvent "TickyCounterSample"
    TickyBeginSample{}          -> mkEvent "TickyBeginSample"
