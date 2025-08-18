{-# LANGUAGE NumericUnderscores #-}

module Pings (main) where

-- Internal
import ClickHaskell

-- GHC included
import Control.Monad (replicateM_)
import Data.Time (diffUTCTime, getCurrentTime)


main :: IO ()
main = do
  conn <- openConnection defaultConnectionArgs
  calcTime $ replicateM_ 500_000 $ ping conn


calcTime :: IO a -> IO ()
calcTime io = do
  before <- getCurrentTime
  _ <- io
  after <- getCurrentTime
  print $ after `diffUTCTime` before
