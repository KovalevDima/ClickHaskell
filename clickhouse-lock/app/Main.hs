module Main where

import ClickHouse.Lock (runClickHouseLock)

main :: IO ()
main = runClickHouseLock
