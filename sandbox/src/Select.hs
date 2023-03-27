{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}
module Select where

import ClickHaskell           (HttpChClient, initClient, ChCredential (..), httpStreamChSelect)
import ClickHaskell.ChTypes   (ChString, ChInt64, ChUUID, ChDateTime)
import ClickHaskell.TableDsl  (HasChSchema)
import Network.HTTP.Client    as H (newManager, defaultManagerSettings)

import GHC.Generics           (Generic)
import Control.Concurrent     (threadDelay)


-- 1. Create our schema haskell representation

data Example = Example
  { channel_name :: ChString
  , clientId     :: ChInt64
  , someField    :: ChDateTime
  , someField2   :: ChUUID
  }
  deriving (Generic, HasChSchema, Show)

select :: IO ()
select = do

  -- 2. Init clienthttpStreamChInsert client bufferData
  httpManager <- H.newManager H.defaultManagerSettings
  client <- initClient @HttpChClient
    (Just httpManager)
    (ChCredential "default" "" "http://localhost:8123")

  dat <- httpStreamChSelect @Example client "example" "example"
  mapM_ print dat

  threadDelay 15_000_000
