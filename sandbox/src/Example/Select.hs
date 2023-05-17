{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeOperators
  , ScopedTypeVariables
#-}

module Example.Select where

-- Internal dependencies
import ClickHaskell
import Example      (ExampleTable, ExampleData)


select :: IO ()
select = do

  -- 1. Init http client
  client <- initClient
    @HttpChClient
    (ChCredential "default" "" "http://localhost:8123")
    (Just $ setHttpClientTimeout 60 defaultHttpClientSettings)

  -- 2. Create database and table
  createDatabaseIfNotExists @"example" client
  createTableIfNotExists @(InDatabase "example" ExampleTable) client

  -- 3. Perform select
  print "Reading data"
  dat <- case someSymbolVal "text\t" of
    (SomeSymbol (Proxy :: Proxy var)) ->
      httpStreamChSelect @(("string" `SampledBy` EqualityWith var) ExampleData) @(InDatabase "example" ExampleTable) client

  -- 4. Handle data
  mapM_ print dat
