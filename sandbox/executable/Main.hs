{-# LANGUAGE
    NumericUnderscores
#-}

module Main
  ( main
  ) where

import Bench (benchExecutable, BenchSettings(..))

settings :: BenchSettings
settings = BenchSettings
  { sBufferSize        = 5_000_000
  , sConcurentWriters  = 500
  , sRowsPerWriter     = 100_000
  , sMsBetweenWrites   = 10
  , sMsBetweenChWrites = 1_000_000
  }

main :: IO ()
main = benchExecutable settings
