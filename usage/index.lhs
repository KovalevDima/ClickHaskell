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

ClickHaskell appropriates unique approach

<pre><code data-lang="haskell" class="haskell"
>data ExampleData = MkExampleData
  { a1 :: Int64
  , a3 :: UInt32
  , a2 :: ChString
  }
  deriving (Generic, Show)

type ExampleCols =
 '[ Column "a1" Int64
  , Column "a2" ChString
  , Column "a3" (DateTime "")
  ]

{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving instance ClickHaskell ExampleCols ExampleData
</code></pre>



<h3>View</h3>

<pre><code data-lang="haskell" class="haskell"
>exampleView :: Connection -> IO [ExampleData]
exampleView connection = 
  mconcat <$>
    selectFromView
      @(View "exampleParametrizedView" ExampleCols ExampleParams)
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
    @(Table "exampleWriteRead" ExampleCols)
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
      "SELECT CAST(5, 'UInt8') as num LIMIT 5;"
      pure
</code></pre>



<h3>Wrap everything together</h3>

<pre><code data-lang="haskell" class="haskell"
>main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs

  selectRes <- exampleSelect connection
  viewRes <- exampleView connection

  exampleInsert connection (selectRes <> viewRes)
</code></pre>
