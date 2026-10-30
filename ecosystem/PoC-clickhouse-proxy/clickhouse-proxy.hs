module Main where

import ClickHouseProxy.TcpServer (runTcpServer)

main :: IO ()
main = do
  putStrLn "Starting server"
  runTcpServer
