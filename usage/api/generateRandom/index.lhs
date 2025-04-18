<nav style="display: flex; flex-direction: row; align-items: center;">
    <p>Library API</p>
    <ul type="square">
        <li><a href="/#/usage/api/generateRandom">/generateRandom</a></li>
        <li><a href="/#/usage/api/insertInto">/insertInto</a></li>
        <li><a href="/#/usage/api/select">/select</a></li>
        <li><a href="/#/usage/api/selectFromView">/selectFromView</a></li>
    </ul>
</nav>

<h1>generateRandom</h1> is a special type ClickHouse DBMS table<br>
function which generatesRandom data for given schema
<br>
<br>

You can use this function in Haskell with given wrapper

<pre><code data-lang="haskell" class="haskell"
>{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DerivingStrategies
  , OverloadedStrings
#-}

import ClickHaskell
  ( ReadableFrom, generateRandom
  , openNativeConnection, defaultCredentials
  , Columns, Column
  , ChString, UUID, DateTime
  )
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word32)
import GHC.Generics (Generic)

main :: IO ()
main = do
  connection <- openNativeConnection defaultCredentials
  _ <-
    generateRandom
      @ExampleColumns
      @ExampleData
      connection
      (1, 5, 10)
      1
      pure
  pure ()

{- Before GHC 9.8 its better to use standalone deriving
   since type errors occures exact on deriving declaration.
-}
deriving instance ReadableFrom (Columns ExampleColumns) ExampleData

type ExampleColumns =
 '[ Column "a1" Int64
  , Column "a2" ChString
  , Column "a3" (DateTime "")
  , Column "a4" UUID
  ]

data ExampleData = MkExampleData
  { a1 :: Int64
  , a2 :: ByteString
  , a4 :: UUID
  , a3 :: Word32
  }
  deriving (Generic, Show)
</code></pre>
