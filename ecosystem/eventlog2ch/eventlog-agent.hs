module Main (main) where

import Control.Concurrent.Async (Concurrently (..))
import System.Environment (lookupEnv, getEnv)

import EventLogAgent.ClickHouse
import EventLogAgent.EventlogNetworking

main :: IO ()
main = do
  clickHouseHost <- lookupEnv "CLICKHOUSE_HOST"
  clickHouseDb   <- lookupEnv "CLICKHOUSE_DB"
  clickHouseUser <- lookupEnv "CLICKHOUSE_USER"
  clickHousePass <- lookupEnv "CLICKHOUSE_PASS"
  socketPath     <- getEnv "EVENTLOG_SOCKET_PATH"

  MkEventlogNetworking{eventlogProducer, ..} <- initEventlogProducer MkEventlogNetworkingArgs{..}
  MkClickHouseWriter{clickHouseWriter} <- initClickHouseWriter MkClickHouseWriterArgs{..}

  runConcurrently
    $ pure ()
    *> clickHouseWriter
    *> eventlogProducer
