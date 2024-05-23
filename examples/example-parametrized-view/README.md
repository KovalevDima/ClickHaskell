# ClickHaskell: parametrized view example

Example of using a parameterized view.
To try out this example in repl: setup [development environment](https://github.com/GetShopTV/ClickHaskell#development-environment) and then run

```bash
cabal repl test:example-parametrized-view
```

## Code Example

```haskell
{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
#-}

-- Internal
import ClickHaskell.Client
  ( interpretClient, initClient, setHttpClientTimeout
  , Reading, HttpChClient, ChCredential(..)
  )
import ClickHaskell.Generics (ReadableFrom)
import ClickHaskell.Tables
  ( interpretTable, mkParameter
  , View, Parameter, Column
  )
import ClickHouse.DbTypes (ChUInt32, ChString)


-- GHC included
import Data.ByteString (StrictByteString)
import GHC.Generics    (Generic)

main :: IO ()
main = do
  client <- exampleClient
  records <- readParametrizedView client
  mapM_ print records

exampleClient :: IO HttpChClient
exampleClient = initClient
  exampleCredentials
  (Just $ setHttpClientTimeout 500_000)

exampleCredentials :: ChCredential
exampleCredentials = MkChCredential "default" "" "http://localhost:8123" "default"

readParametrizedView :: HttpChClient -> IO [ExampleViewRecord]
readParametrizedView client = interpretClient
  @(Reading ExampleViewRecord -> ExampleView)
  client
  (interpretTable @ExampleView (mkParameter 2) (mkParameter 6))

type ExampleView =
  View
    "exampleParametrizedView"
   '[ Column "a3" ChString
    ]
   '[ Parameter "a1MoreThan" ChUInt32
    , Parameter "a2LessThan" ChUInt32
    ]

newtype ExampleViewRecord = MkExampleViewRecord
  { a3 :: StrictByteString 
  }
  deriving (Generic, Show)

instance ReadableFrom ExampleView ExampleViewRecord
```

## ClickHouse

ClickHouse script used for this example:

```sql
CREATE TABLE exampleViewTable
(
    `a1` UInt32,
    `a2` UInt32,
    `a3` String
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ();

CREATE VIEW exampleParametrizedView AS
SELECT *
FROM exampleViewTable
WHERE (a1 > {a1MoreThan:UInt32}) AND  (a2 < {a2LessThan:UInt32});

INSERT INTO exampleViewTable (*) VALUES
(1, 9, 'a'),
(3, 7, 'b'),
(5, 5, 'c');
```
