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

module IntegrationTests
  ( runIntegrationTests
  ) where


-- Internal
import ClickHaskell.Client
  ( HttpChClient(..)
  )
import IntegrationTests.Serialization (runSerializationTests)


runIntegrationTests :: HttpChClient -> IO ()
runIntegrationTests client = do
  runSerializationTests client
