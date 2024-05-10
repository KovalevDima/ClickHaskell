{-# LANGUAGE
    AllowAmbiguousTypes
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeApplications
  , UndecidableInstances
  , ScopedTypeVariables
#-}

module Main 
  ( main
  ) where


-- Internal
import ClickHaskell.Client
  ( ClientInterpretable(..)
  , HttpChClient(..)
  , IsChClient(..)
  , ChCredential(..)
  )
import IntegrationTests (runIntegrationTests)



main :: IO ()
main = do
  client <-
    initClient
      @HttpChClient
      MkChCredential
        { chLogin="default"
        , chPass=""
        , chUrl="http://localhost:8123"
        , chDatabase="default"
        }
      Nothing

  runIntegrationTests client
