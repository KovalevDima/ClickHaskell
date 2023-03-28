{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}

module WriteTest
  ( writeExample
  , WriteExampleSettings(..)
  ) where

import ClickHaskell           (HttpChClient, initClient, ChCredential (..), createSizedBuffer,
                              writeToSizedBuffer, httpStreamChInsert, BufferSize, forkBufferFlusher)
import ClickHaskell.ChTypes   (ChString, ChInt64, ChUUID, ChDateTime, ToChType(toChType))
import ClickHaskell.TableDsl  (HasChSchema)
import Network.HTTP.Client    as H (newManager, defaultManagerSettings)
import Data.UUID              as UUID (nil)
import Data.Time              (UTCTime(UTCTime), secondsToDiffTime, fromGregorian)

import Data.Text              (Text)
import GHC.Generics           (Generic)
import Control.Monad          (replicateM_, void)
import Control.Exception      (SomeException)
import Control.Concurrent     (threadDelay, forkIO)
import Control.Concurrent.STM (TBQueue)


-- 0. Settings for test

newtype ConcurrentBufferWriters   = ConcurrentBufferWriters   Int deriving newtype (Num)
newtype RowsPerBufferWriter       = RowsPerBufferWriter       Int deriving newtype (Num)
newtype MsBetweenBufferWrites     = MsBetweenBufferWrites     Int deriving newtype (Num)
newtype MsBetweenClickHouseWrites = MsBetweenClickHouseWrites Int deriving newtype (Num)


data WriteExampleSettings = WriteExampleSettings
  { sBufferSize        :: BufferSize
  , sConcurentWriters  :: ConcurrentBufferWriters
  , sRowsPerWriter     :: RowsPerBufferWriter
  , sMsBetweenWrites   :: MsBetweenBufferWrites
  , sMsBetweenChWrites :: MsBetweenClickHouseWrites
  }

-- 1. Create our schema haskell representation

data Example = Example
  { channel_name :: ChString
  , clientId     :: ChInt64
  , someField    :: ChDateTime
  , someField2   :: ChUUID
  }
  deriving (Generic, HasChSchema)

writeExample :: WriteExampleSettings -> IO ()
writeExample (
  WriteExampleSettings
     bufferSize
     (ConcurrentBufferWriters   concurrentBufferWriters)
     (RowsPerBufferWriter       rowsNumber             )
     (MsBetweenBufferWrites     msBetweenBufferWrites  )
     (MsBetweenClickHouseWrites msBetweenChWrites      )
  ) = do

  -- 2. Init clienthttpStreamChInsert client bufferData
  httpManager <- H.newManager H.defaultManagerSettings
  client <- initClient @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    (Just httpManager)

  -- 3. Create buffer 
  (buffer :: TBQueue Example) <- createSizedBuffer bufferSize

  -- 4. Start buffer flusher
  _ <- forkBufferFlusher
    (fromIntegral msBetweenChWrites)
    buffer
    (\(e :: SomeException)-> print e)
    (\bufferData -> void $ httpStreamChInsert client bufferData "test" "example")

  -- 5. Get some data
  let _dataExample = Example
        { channel_name = toChType @ChString   $ ("text"   :: Text)
        , clientId     = toChType @ChInt64      42
        , someField    = toChType @ChDateTime $ UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)
        , someField2 =   toChType @ChUUID       UUID.nil
        }

  -- 6. Write something to buffer
  _threadId <-
    replicateM_ concurrentBufferWriters . forkIO
      $ replicateM_ rowsNumber
        ( (\someData -> do
            -- Writing
            writeToSizedBuffer buffer someData

            threadDelay msBetweenBufferWrites
          )
        _dataExample
        )

  threadDelay 60_000_000
