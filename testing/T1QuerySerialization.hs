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
  , ReadableFrom, select
  , Column, Columns, KnownColumn, DeserializableColumn
  , IsChType(..), ToChType(..)
  , ToQueryPart(..)
  , UInt8, UInt16, UInt32, UInt64
  , Int8, Int16, Int32, Int64
  , ChString, UUID
  )

-- GHC included
import Control.Monad (when)
import Data.ByteString as BS (singleton)
import Data.ByteString.Builder (toLazyByteString)
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
  runTestForType @UUID conn [minBound, toEnum 0, maxBound]
  runTestForType @ChString conn (map (toChType . BS.singleton) [1..255])
  -- ToDo: querySerializationTest @(LowCardinality ChString) connection (map (toChType . BS.singleton) [0..255])
  -- ToDo: querySerializationTest @(ChArray ChString) connection [toChType $ map BS.singleton [0..255]]
  -- ToDo: querySerializationTest @(ChArray ChInt64) connection [toChType [0 :: ChInt64 .. 255]]


runTestForType ::
  forall chType
  .
  ( ToQueryPart chType
  , IsChType chType
  , Eq chType
  , Show chType
  , ReadableFrom (Columns '[Column "testSample" chType]) (TestSample chType)
  )
  =>
  Connection -> [chType] -> IO ()
runTestForType connection testValues = do
  let typeName = chTypeName @chType
  mapM_
    (\chType -> do
      [selectChType] <-
        concat <$>
          select
            @'[Column "testSample" chType]
            @(TestSample chType)
            connection
            (toChType ("SELECT CAST(" <> toQueryPart chType <> ", '" <> typeName <> "') as testSample;"))
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
  ( DeserializableColumn (Column "testSample" chType)
  , KnownColumn (Column "testSample" chType)
  )
  =>
  ReadableFrom (Columns '[Column "testSample" chType]) (TestSample chType)
