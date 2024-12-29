<h2>Testing</h2>

```haskell
module Tests where
```

```haskell
import T1QuerySerialization (t1)
```

1. Builds queries like

    ```sql
    SELECT CAST(5, 'UInt8') as testSample;
    ```
    via **ToQueryPart** type class\
    for every supported type
2. Executes **select**
3. Parses the result
4. Checks if result equals initial value

```haskell
import T2WriteReadEquality (t2)
```
1. Runs **insertInto** of a sample into the all supported types table
2. Runs **selectFrom** from the same table
3. Checks if result equals sample value

<h2>How to run</h2>
You can manually run database and tests:

```bash
nix run .#testing
```

<h2>Main function</h2>

```haskell
import ClickHaskell (ChCredential(..), openNativeConnection, defaultCredentials)
import T3Multithreading (t3)

main :: IO ()
main = do
  connection <- openNativeConnection defaultCredentials
  mapM_
    (\runner -> runner connection) 
    [t1,t2,t3]
```
