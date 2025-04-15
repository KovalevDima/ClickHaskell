module ChEventlogWriter where

import GHC.Eventlog.Socket (start)
import ClickHaskell

chEventlogWrite :: IO Connection -> FilePath -> IO ()
chEventlogWrite initConn socketPath = do
  _ <- initConn
  start socketPath
