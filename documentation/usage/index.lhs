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
We need to define our types

<pre><code data-lang="haskell" class="haskell"
>data ExampleData = MkExampleData
  { a1 :: Int32
  , a2 :: ChString
  , a3 :: UInt32
  }
  deriving (Generic, Show)

type ExampleCols =
 '[ Column "a1" Int32
  , Column "a2" ChString
  , Column "a3" (DateTime "")
  ]
</code></pre>
<br>

and generate client side code

<pre><code data-lang="haskell" class="haskell"
>{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving instance ClickHaskell ExampleCols ExampleData
</code></pre>
<br>

<p>
  Also we should create the table
</p>


<pre><code data-lang="haskell" class="haskell"
>createTable :: Connection -> IO ()
createTable connection =
  command
    connection
    "CREATE TABLE IF NOT EXISTS exampleTable ( \
    \  `a1` Int32, \
    \  `a2` String, \
    \  `a3` DateTime, \
    \) \
    \ENGINE = MergeTree \
    \PARTITION BY () \
    \ORDER BY ();"
</code></pre>
<br>

<h3>Create table, read and then write</h3>

<pre><code data-lang="haskell" class="haskell"
>main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs

  createTable connection
  let
    selectQuery =
      unsafeMkSelect
        @ExampleCols
        @ExampleData
        (\_cols -> " SELECT \
          \   defaultValueOfTypeName('Int32') as a1,   \
          \   defaultValueOfTypeName('String') as a2,  \
          \   defaultValueOfTypeName('DateTime') as a3 \
          \ LIMIT 5;"
        )
    addSettingsToQuery =
      passSettings (
        addTestSetting .
        id
      )

  results <-
    select
      (addSettingsToQuery selectQuery)
      connection
      pure

  insert
    (intoTable
      @"exampleTable"
      @ExampleCols
      @ExampleData
    )
    connection
    (mconcat results)
</code></pre>
