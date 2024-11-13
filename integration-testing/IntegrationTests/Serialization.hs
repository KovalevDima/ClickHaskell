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

module IntegrationTests.Serialization
  ( runSerializationTest
  ) where

-- Internal
import ClickHaskell.DbTypes
  ( IsChType(..), ToChType(..), ToQueryPart(..)
  , ChUInt64, ChInt64, ChUInt32, ChInt32
  , ChString
  , ChArray
  )
import ClickHaskell
  ( ChCredential(..), Connection(..), openNativeConnection
  , select, Column, Columns, ReadableFrom, FromChType
  )
import ClickHaskell.Deserialization (Deserializable)

-- External
import Network.HTTP.Client as H (Manager, newManager, defaultManagerSettings)
import Control.Monad (void, when)
import Data.ByteString as BS (singleton)
import Data.ByteString.Char8 as BS8 (takeWhile)
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Generics (Generic)
import Debug.Trace (traceShowId)


runSerializationTest ::
  forall chType
  .
  ( ToQueryPart chType
  , Eq chType
  , KnownSymbol (ToChTypeName chType)
  , Show chType
  , ReadableFrom (Columns '[Column "testSample" chType]) (TestSample chType)
  )
  =>
  Connection -> [chType] -> IO ()
runSerializationTest connection testValues = do
  mapM_
    (\chType -> do
      selectChType <-
        head <$>
          select
            @'[Column "testSample" chType]
            @(TestSample chType)
            connection
            (toChType (traceShowId $ "SELECT CAST(" <> toQueryPart chType <> ", '" <> chTypeName @chType <> "') as testSample;"))

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
  ( IsChType chType
  , KnownSymbol (ToChTypeName chType)
  , FromChType chType chType
  , Deserializable chType
  )
  =>
  ReadableFrom (Columns '[Column "testSample" chType]) (TestSample chType)
