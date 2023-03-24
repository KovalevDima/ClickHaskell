# ClickHaskell
### Haskell interface for integration with ClickHouse

Package still under developing and doesn't have stable interface 


### Example of application that uses library functionality for tests

```haskell
{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}
module Example where

import ClickHaskell           (HttpChClient, initClient, ChCredential (..), createSizedBuffer,
                              writeToBuffer, httpStreamChInsert, forkBufferFlusher)
import ClickHaskell.ChTypes   (ChString, ChInt64, ChUUID, ChDateTime, ToChType(toChType))
import ClickHaskell.TableDsl  (HasChSchema)
import Network.HTTP.Client    as H (newManager, defaultManagerSettings)
import Data.UUID              as UUID (nil)
import Data.Time              (UTCTime(UTCTime), secondsToDiffTime, fromGregorian)

import Data.Text              (Text)
import GHC.Generics           (Generic)
import Control.Monad          (void)
import Control.Exception      (SomeException)
import Control.Concurrent     (threadDelay)
import Control.Concurrent.STM (TBQueue)


-- 1. Create our schema haskell representation

data Example = Example
  { channel_name :: ChString
  , clientId     :: ChInt64
  , someField    :: ChDateTime
  , someField2   :: ChUUID
  }
  deriving (Generic, HasChSchema)

writeExample :: IO ()
writeExample = do

  -- 2. Init clienthttpStreamChInsert client bufferData
  httpManager <- H.newManager H.defaultManagerSettings
  client <- initClient @HttpChClient
    (Just httpManager)
    (ChCredential "default" "" "http://localhost:8123")

  -- 3. Create buffer 
  (buffer :: TBQueue Example) <- createSizedBuffer 500_000

  -- 4. Start buffer flusher
  _ <- forkBufferFlusher
    5_000_000
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

  -- 6. Write data to buffer
  writeToBuffer buffer _dataExample

  threadDelay 60_000_000
```
