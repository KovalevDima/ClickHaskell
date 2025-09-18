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

module T1QuerySerialization
  ( t1
  ) where

-- Internal
import ClickHaskell
  ( Connection(..)
  , ClickHaskell, select, unsafeMkSelect
  , Column, KnownColumn, SerializableColumn
  , IsChType(..), ToChType(..)
  , ToQueryPart(..)
  , UInt8, UInt16, UInt32, UInt64
  , Int8, Int16, Int32, Int64
  , ChString, UUID, DateTime, UInt128, UInt256, Array -- , DateTime64
  )

-- GHC included
import Control.Monad (when)
import Data.ByteString as BS (singleton)
import Data.ByteString.Char8 as BS8 (pack)
import Data.ByteString.Builder (toLazyByteString, byteString)
import GHC.Generics (Generic)


t1 :: Connection -> IO ()
t1 conn = do
  runTestForType @Int8 conn [minBound, toEnum 0, maxBound]
  runTestForType @Int16 conn [minBound, toEnum 0, maxBound]
  runTestForType @Int32 conn [minBound, toEnum 0, maxBound]
  runTestForType @Int64 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt8 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt16 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt32 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt64 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt128 conn [minBound, toEnum 0, maxBound]
  runTestForType @UInt256 conn [minBound, toEnum 0, maxBound]
  runTestForType @UUID conn [minBound, toEnum 0, maxBound]
  runTestForType @(DateTime "") conn [minBound, toEnum 0, maxBound]
  -- runTestForType @(DateTime64 0 "") conn [minBound, toEnum 0, maxBound]
  runTestForType @ChString conn (map (toChType . BS.singleton) [1..255])
  -- ToDo: runTestForType @(LowCardinality ChString) connection (map (toChType . BS.singleton) [0..255])
  -- runTestForType @(Array ChString) conn [toChType $ map BS.singleton [0..255]]
  -- ToDo: runTestForType @(ChArray ChInt64) connection [toChType [0 :: ChInt64 .. 255]]


runTestForType ::
  forall chType
  .
  ( ToQueryPart chType
  , IsChType chType
  , Eq chType
  , Show chType
  , ClickHaskell '[Column "testSample" chType] (TestSample chType)
  )
  =>
  Connection -> [chType] -> IO ()
runTestForType connection testValues = do
  let typeName = (byteString . BS8.pack) (chTypeName @chType)
  mapM_
    (\chType -> do
      [selectChType] <-
        concat <$>
          select
            (unsafeMkSelect
              @'[Column "testSample" chType]
              @(TestSample chType)
              (\_cols -> "SELECT CAST(" <> toQueryPart chType <> ", '" <> typeName <> "') as testSample;")
            )
            connection
            pure

      (when (chType /= testSample selectChType) . error)
        (  "Deserialized value of type " <> show (toLazyByteString typeName) <> " unmatched:"
        <> " Expected: " <> show chType
        <> ". But got: " <> show selectChType <> "."
        )
    )
    testValues

  print (toLazyByteString typeName <> ": Ok")


data TestSample chType = MkTestSample {testSample :: chType}
  deriving (Generic, Show)


instance
  ( SerializableColumn (Column "testSample" chType)
  , KnownColumn (Column "testSample" chType)
  )
  =>
  ClickHaskell '[Column "testSample" chType] (TestSample chType)
