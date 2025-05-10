<h1>selectFromView</h1>

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
  ( ClickHaskell, select
  , Column
  , openConnection, defaultConnectionArgs
  , UInt8
  )
import GHC.Generics (Generic)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
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
deriving anyclass instance ClickHaskell ExampleColumns ExampleTableRecord

type ExampleColumns =
   '[ Column "num" UInt8
    ]

newtype ExampleTableRecord = MkExampleTableRecord
  { num :: UInt8
  }
  deriving (Generic, Show)
</code>
</pre></div>
