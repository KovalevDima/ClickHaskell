<h2>Testing</h2>

<pre><code data-lang="haskell" class="haskell"
>{-# LANGUAGE OverloadedStrings #-}
module Tests where
</code></pre>


<pre><code data-lang="haskell" class="haskell"
>import T1QuerySerialization (t1)
</code></pre>

1. Builds queries like
  <pre><code data-lang="sql" class="sql">SELECT CAST(5, 'UInt8') as testSample;</code></pre>
  via <b>ToQueryPart</b> type class for every supported type<br>
2. Executes <b>select</b><br>
3. Parses the result<br>
4. Checks if result equals initial value

<pre><code data-lang="haskell" class="haskell"
>import T2WriteReadEquality (t2)
</code></pre>

1. Runs <b>insertInto</b> of a sample into the all supported types table<br>
2. Runs <b>selectFrom</b> from the same table<br>
3. Checks if result equals sample value


<pre><code data-lang="haskell" class="haskell"
>import T3Multithreading (t3)
</code></pre>
1. Runs 10000 concurrent queries via single connection


<pre><code data-lang="haskell" class="haskell"
>import T4MissmatchErrors (t4)
</code></pre>
1. Runs queries with types and names missmatch and handles error


<h2>How to run</h2>
You can manually run database and tests:

<pre><code data-lang="bash" class="bash"
>nix run .#testing
</code></pre>


<h2>Main function</h2>

<pre><code data-lang="haskell" class="haskell"
>import ClickHaskell (openConnection, defaultConnectionArgs)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  mapM_
    (\runner -> runner connection) 
    [t1,t2,t3,t4]
</code></pre>
