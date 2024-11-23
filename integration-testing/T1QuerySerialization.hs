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
  ( querySerializationTest
  ) where

-- Internal
import ClickHaskell
  ( ChCredential(..), Connection(..), openNativeConnection
  , ReadableFrom, select
  , Column, Columns, KnownColumn, DeserializableColumn
  )
import ClickHaskell.DbTypes
  ( IsChType(..), ToChType(..), FromChType
  , ToQueryPart(..)
  , ChUInt32, ChUInt64
  , ChInt32, ChInt64
  , ChString, ChArray
  )
import ClickHaskell.DeSerialization (Deserializable)

-- GHC included
import Control.Monad (void, when)
import Data.ByteString as BS (singleton)
import Data.ByteString.Char8 as BS8 (takeWhile)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)


querySerializationTest ::
  forall chType
  .
  ( ToQueryPart chType
  , Eq chType
  , Show chType
  , ReadableFrom (Columns '[Column "testSample" chType]) (TestSample chType)
  )
  =>
  Connection -> [chType] -> IO ()
querySerializationTest connection testValues = do
  mapM_
    (\chType -> do
      selectChType <-
        head <$>
          select
            @'[Column "testSample" chType]
            @(TestSample chType)
            connection
            (toChType ("SELECT CAST(" <> toQueryPart chType <> ", '" <> chTypeName @chType <> "') as testSample;"))

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
