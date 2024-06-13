{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ClickHaskell.Client (ChCredential(..))
import IntegrationTests (runIntegrationTests)

main :: IO ()
main =
  runIntegrationTests
    MkChCredential
      { chLogin="default"
      , chPass=""
      , chUrl="http://localhost:8123"
      , chDatabase="default"
      }
