Lets write simple executable with basic usage example
<br>
<br>
Before we start we need to define module
<pre><code data-lang="haskell" class="haskell"
>{-# LANGUAGE
  DataKinds,
  DeriveAnyClass,
  DeriveGeneric,
  OverloadedStrings,
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeApplications,
  StandaloneDeriving
#-}

module Main where

import ClickHaskell
import GHC.Generics (Generic)
</code></pre>
<br>

ClickHaskell provides unique API in area of DBMS clients<br>
<br>
We need to define our types and derive ClickHaskell instance

<pre><code data-lang="haskell" class="haskell"
>type ExampleCols =
 '[ Column "a1" Int32
  , Column "a2" ChString
  , Column "a3" (DateTime "")
  ]

data ExampleData = MkExampleData
  { a1 :: Int32
  , a2 :: ChString
  , a3 :: UInt32
  }
  deriving (Generic, ClickHaskell ExampleCols)
</code></pre>
<br>

<p>
  Define queries
</p>


<pre><code data-lang="haskell" class="haskell"
>createTableQuery :: ChString
createTableQuery  =
  "CREATE TABLE IF NOT EXISTS exampleTable ( \
  \  `a1` Int32, \
  \  `a2` String, \
  \  `a3` DateTime, \
  \) \
  \ENGINE = MergeTree \
  \PARTITION BY () \
  \ORDER BY ();"

selectQuery :: Select ExampleCols ExampleData
selectQuery =
  unsafeMkSelect @ExampleCols @ExampleData
    (\_cols ->
        "SELECT \
        \  defaultValueOfTypeName('Int32') as a1,   \
        \  defaultValueOfTypeName('String') as a2,  \
        \  defaultValueOfTypeName('DateTime') as a3 \
        \ "
    )

insertQuery :: Insert ExampleCols ExampleData
insertQuery = intoTable @"exampleTable" @ExampleCols @ExampleData
</code></pre>
<br>

<h3>Create table, read and then write</h3>

<pre><code data-lang="haskell" class="haskell"
>main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs

  command connection createTableQuery

  batches <- select selectQuery connection (\batch -> pure batch)

  insert insertQuery connection (mconcat batches)
</code></pre>
