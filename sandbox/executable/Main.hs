{-# LANGUAGE
  NumericUnderscores
#-}
module Main
  ( main
  ) where

import WriteTest (writeExample, WriteExampleSettings(..))

settings :: WriteExampleSettings
settings = WriteExampleSettings
  { sBufferSize        = 5_000_000
  , sConcurentWriters  = 500
  , sRowsPerWriter     = 100_000
  , sMsBetweenWrites   = 10
  , sMsBetweenChWrites = 1_000_000
  }

main :: IO ()
main = writeExample settings
