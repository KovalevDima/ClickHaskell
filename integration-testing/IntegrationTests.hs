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
  ( main
  ) where

-- Internal
import ClickHaskell.Client
  ( ChCredential(..)
  )
import IntegrationTests.Serialization (runSerializationTests)
import IntegrationTests.WriteReadEquality (runWriteReadEqualityTest)

main :: IO ()
main =
  runIntegrationTests
    MkChCredential
      { chLogin="default"
      , chPass=""
      , chUrl="http://localhost:8123"
      , chDatabase="default"
      }


runIntegrationTests :: ChCredential -> IO ()
runIntegrationTests cred = do
  runSerializationTests cred
  runWriteReadEqualityTest cred
