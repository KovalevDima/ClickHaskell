<h1>usage</h1>

Lets write simple executable with generic usage examples
<br>
<br>
Before we start we need to define module
<pre><code data-lang="haskell" class="haskell"
>{-# LANGUAGE DataKinds, DeriveAnyClass, OverloadedStrings #-}

module Main where 

import ClickHaskell
import GHC.Generics (Generic)
</code></pre>



<h3>General preparation</h3>

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

and generate client side code

<pre><code data-lang="haskell" class="haskell"
>{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving instance ClickHaskell ExampleCols ExampleData
</code></pre>

<h3>Command</h3>

Also we should create the table and view

<pre><code data-lang="haskell" class="haskell"
>createView :: Connection -> IO ()
createView connection =
  command
    connection
    "CREATE OR REPLACE VIEW exampleView \
    \AS SELECT * \
    \FROM generateRandom('a1 Int32, a2 String, a3 DateTime', 1, 10, 2) \
    \WHERE (a1 > {a1MoreThan:Int32}) AND (a1 < {a1LessThan:Int32}) \
    \LIMIT 5;"

createTable :: Connection -> IO ()
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



<h3>View</h3>

<pre><code data-lang="haskell" class="haskell"
>exampleView :: Connection -> IO [ExampleData]
exampleView connection = 
  mconcat <$>
    selectFromView
      @(View "exampleView" ExampleCols ExampleParams)
      @ExampleData
      connection
      exampleParams
      pure

exampleParams :: Parameters '[] -> Parameters ExampleParams
exampleParams =
  ( parameter @"a1MoreThan" @Int32 ((-100_000) :: Int32)
  . parameter @"a1LessThan" @Int32 ((100_000) :: Int32)
  )

type ExampleParams =
 '[ Parameter "a1MoreThan" Int32
  , Parameter "a1LessThan" Int32
  ]
</code></pre>



<h3>Insert</h3>

<pre><code data-lang="haskell" class="haskell"
>exampleInsert :: Connection -> [ExampleData] -> IO ()
exampleInsert connection x =
  insertInto
    @(Table "exampleTable" ExampleCols)
    @ExampleData
    connection
    x
</code></pre>



<h3>Select</h3>

<pre><code data-lang="haskell" class="haskell"
>exampleSelect :: Connection -> IO [ExampleData]
exampleSelect connection =
  mconcat <$>
    select
      @ExampleCols
      @ExampleData
      connection
      "SELECT CAST(5, 'Int32') as a1, 'hello' as a2, CAST(5, 'DateTime') as a3 LIMIT 5;"
      pure
</code></pre>



<h3>Connection</h3>

<pre><code data-lang="haskell" class="haskell"
>main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs

  createView connection
  createTable connection

  selectRes <- exampleSelect connection
  viewRes <- exampleView connection

  exampleInsert connection (selectRes <> viewRes)
</code></pre>
