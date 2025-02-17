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
  ( ChCredential(..), Connection(..), openNativeConnection
  , ReadableFrom, select
  , Column, Columns, KnownColumn, DeserializableColumn
  , IsChType(..), ToChType(..), FromChType
  , ToQueryPart(..)
  , ChUInt8, ChUInt16, ChUInt32, ChUInt64
  , ChInt8, ChInt16, ChInt32, ChInt64
  , ChString, ChArray, ChUUID
  )

-- GHC included
import Control.Monad (void, when)
import Data.ByteString as BS (singleton)
import Data.ByteString.Char8 as BS8 (takeWhile)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)


t1 :: Connection -> IO ()
t1 conn = do
  runTestForType @ChInt8 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChInt16 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChInt32 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChInt64 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChUInt8 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChUInt16 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChUInt32 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChUInt64 conn [minBound, toEnum 0, maxBound]
  runTestForType @ChUUID conn [minBound, toEnum 0, maxBound]
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
  mapM_
    (\chType -> do
      selectChType <-
        head . head <$>
          select
            @'[Column "testSample" chType]
            @(TestSample chType)
            connection
            (toChType ("SELECT CAST(" <> toQueryPart chType <> ", '" <> chTypeName @chType <> "') as testSample;"))
            pure

      (when (chType /= testSample selectChType) . error)
        (  "Deserialized value of type " <> show (chTypeName @chType) <> " unmatched:"
        <> " Expected: " <> show chType
        <> ". But got: " <> show selectChType <> "."
        )
    )
    testValues

  print (chTypeName @chType <> ": Ok")


data TestSample chType = MkTestSample {testSample :: chType}
  deriving (Generic, Show)


instance
  ( DeserializableColumn (Column "testSample" chType)
  , KnownColumn (Column "testSample" chType)
  )
  =>
  ReadableFrom (Columns '[Column "testSample" chType]) (TestSample chType)
