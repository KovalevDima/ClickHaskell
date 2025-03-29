<h2>Testing</h2>

<pre><code data-lang="haskell" class="haskell"
>module Tests where
</code></pre>


<pre><code data-lang="haskell" class="haskell"
>import T1QuerySerialization (t1)
</code></pre>

1. Builds queries like

    ```sql
    SELECT CAST(5, 'UInt8') as testSample;
    ```
    via **ToQueryPart** type class\
    for every supported type
2. Executes **select**
3. Parses the result
4. Checks if result equals initial value


<pre><code data-lang="haskell" class="haskell"
>import T2WriteReadEquality (t2)
</code></pre>
1. Runs **insertInto** of a sample into the all supported types table
2. Runs **selectFrom** from the same table
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
>import ClickHaskell (openNativeConnection, defaultCredentials)

main :: IO ()
main = do
  connection <- openNativeConnection defaultCredentials
  mapM_
    (\runner -> runner connection) 
    [t1,t2,t3,t4]
</code></pre>
