{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , NumericUnderscores
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , ScopedTypeVariables
#-}
{-# LANGUAGE TypeOperators #-}
module Example.Select where

import ClickHaskell              (HttpChClient, initClient, ChCredential (..), httpStreamChSelect)
import ClickHaskell.TableDsl     (InDatabase, SampledBy, EqualityWith)

-- 1. Describe table and queryable data
import Example (ExampleTable, ExampleData)
import GHC.TypeLits (SomeSymbol(SomeSymbol), someSymbolVal)
import Data.Data (Proxy(Proxy))





select :: IO ()
select = do

  -- 2. Init http client
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    Nothing

  -- 3. Perform select
  dat <- case someSymbolVal "" of (SomeSymbol (Proxy :: Proxy var)) -> httpStreamChSelect @(("fieldName" `SampledBy` EqualityWith var) ExampleData) @(InDatabase "example" ExampleTable) client

  -- 4. Handle data
  mapM_ print dat
