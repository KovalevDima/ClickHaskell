<nav style="display: flex; flex-direction: row; align-items: center;">
    <p>Library API</p>
    <ul type="square">
        <li><a href="/#/usage/api/generateRandom">/generateRandom</a></li>
        <li><a href="/#/usage/api/insertInto">/insertInto</a></li>
        <li><a href="/#/usage/api/select">/select</a></li>
        <li><a href="/#/usage/api/selectFromView">/selectFromView</a></li>
    </ul>
</nav>

<h1>select</h1>

The <b>select</b> is a wrapper for generic select queries.<br>
Such as

<pre><code class="sql" data-lang="sql"
>SELECT CAST(5, 'UInt8') as num LIMIT 5;
</code></pre>

<pre style="white-space: preserve wrap;">
<code data-lang="haskell" class="haskell"
>{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DerivingStrategies
  , OverloadedStrings
#-}

module Main where

import ClickHaskell
  ( ReadableFrom, select
  , Column, Columns
  , openNativeConnection, defaultCredentials
  , UInt8
  )
import GHC.Generics (Generic)

main :: IO ()
main = do
  connection <- openNativeConnection defaultCredentials
  _ <-
    select
      @ExampleColumns
      @ExampleTableRecord
      connection
      "SELECT CAST(5, 'UInt8') as num LIMIT 5;"
      print
  pure ()

{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving anyclass instance ReadableFrom (Columns ExampleColumns) ExampleTableRecord

type ExampleColumns =
   '[ Column "num" UInt8
    ]

newtype ExampleTableRecord = MkExampleTableRecord
  { num :: UInt8
  }
  deriving (Generic, Show)
</code>
</pre></div>
