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

module IntegrationTests.QuerySerialization
  ( querySerializationTest
  ) where

-- Internal
import ClickHaskell
  ( ChCredential(..), Connection(..), openNativeConnection
  , ReadableFrom, select
  )
import ClickHaskell.DbTypes
  ( IsChType(..), ToChType(..), FromChType
  , ToQueryPart(..)
  , ChUInt32, ChUInt64
  , ChInt32, ChInt64
  , ChString, ChArray
  , Column, Columns
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
  , KnownSymbol (ToChTypeName chType)
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
  ( IsChType chType
  , KnownSymbol (ToChTypeName chType)
  , FromChType chType chType
  , Deserializable chType
  )
  =>
  ReadableFrom (Columns '[Column "testSample" chType]) (TestSample chType)
