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
import IntegrationTests.WriteReadEquality (runWriteReadEqualityTest)


runIntegrationTests :: HttpChClient -> IO ()
runIntegrationTests client = do
  runSerializationTests client
  runWriteReadEqualityTest client
