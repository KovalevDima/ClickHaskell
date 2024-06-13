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
module IntegrationTests.Serialization
  ( runSerializationTests
  ) where


-- Internal
import ClickHouse.DbTypes 
  ( Deserializable(..), IsChType(..), ToChType(..), ToQueryPart(..)
  , ChUInt64, ChInt64, ChUInt32, ChInt32
  , ChString
  )
import ClickHaskell.Client (ChCredential(..), runStatement)


-- External
import Network.HTTP.Client as H (Manager, newManager, defaultManagerSettings)
import Control.Monad (when)
import Data.ByteString         as BS (takeWhile, singleton)
import GHC.TypeLits (KnownSymbol)



runSerializationTests :: ChCredential -> IO ()
runSerializationTests client = do
  manager <- newManager defaultManagerSettings
  runSerializationTest @ChInt32 manager client
  runSerializationTest @ChInt64 manager client
  runSerializationTest @ChUInt32 manager client
  runSerializationTest @ChUInt64 manager client
  -- runSerializationTest @ChString client


runSerializationTest ::
  forall chType
  .
  ( ToQueryPart chType
  , Eq chType
  , Deserializable chType
  , HasTestValues chType
  , KnownSymbol (ToChTypeName chType)
  , Show chType
  )
  =>
  Manager -> ChCredential -> IO ()
runSerializationTest manager chCred = do
  mapM_
      (\chType -> do
        selectChType <- runStatement manager chCred ("SELECT " <> toQueryPart chType)
        let deserializedChType = deserialize . BS.takeWhile (/= 10) $ selectChType
  
        (when (chType /= deserializedChType) . error)
          (  "Deserialized value of type " <> show (chTypeName @chType) <> " unmatched:"
          <> " Expected: " <> show chType
          <> ". But got: " <> show deserializedChType <> "."
          )
        
      )
      (testValues :: [chType])

  print (chTypeName @chType <> ": Ok")





class HasTestValues chType
  where
  testValues :: [chType]

instance {-# OVERLAPPABLE #-}
  ( Bounded boundedEnum
  , Enum boundedEnum
  )
  =>
  HasTestValues boundedEnum
  where
  testValues :: [boundedEnum]
  testValues = [minBound, toEnum 0, maxBound]

instance HasTestValues ChString
  where
  testValues = map (toChType . BS.singleton) [1..255]
