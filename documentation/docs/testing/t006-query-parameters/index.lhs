
```haskell
{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeApplications
  , UndecidableInstances
  , ScopedTypeVariables
#-}

module Main (main) where

import ClickHaskell
import Control.Monad (when)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (byteString)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  runTestForType @Bool connection True
  runTestForType @UInt64 connection 1000
  runTestForType @Int64 connection 1000

  putStrLn "Ok"


runTestForType ::
  forall chType
  .
  ( ClickHaskell '[Column "testSample" chType] (TestSample chType)
  , IsParameterType chType
  , Show chType
  , Eq chType
  , IsChType chType
  )
  =>
  Connection -> chType -> IO ()
runTestForType conn chType = do
  let typeName = (byteString . BS8.pack) (chTypeName @chType)
      query :: Select '[Column "testSample" chType] (TestSample chType)
      query =
        passParameters
          ( id
            . addParameter @chType "param" chType
          )
          (unsafeMkSelect (\_ -> "SELECT {param:" <> typeName <> "} as testSample"))

  [MkTestSample result] <- mconcat <$> select query conn pure

  when (result /= chType) (
    error (
      "Deserialized value of type " <> show typeName <> " unmatched:"
      <> " Expected: " <> show chType
      <> ". But got: " <> show result <> "."
      )
    ) 
  pure ()


data TestSample chType = MkTestSample
  { testSample :: chType
  }
  deriving (Generic, Show)


instance
  ( SerializableColumn (Column "testSample" chType)
  , KnownColumn (Column "testSample" chType)
  )
  =>
  ClickHaskell '[Column "testSample" chType] (TestSample chType)

```
