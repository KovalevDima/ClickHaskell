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
  ( ChCredential
  )
import IntegrationTests.Serialization (runSerializationTests)
import IntegrationTests.WriteReadEquality (runWriteReadEqualityTest)


runIntegrationTests :: ChCredential -> IO ()
runIntegrationTests cred = do
  runSerializationTests cred
  runWriteReadEqualityTest cred
