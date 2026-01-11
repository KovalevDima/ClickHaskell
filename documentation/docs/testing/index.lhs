```haskell
module Tests where
```

---

You can run test local with Nix:
```bash
nix run .#testing
```

---

```haskell
import T1QuerySerialization (t1)
```

- Builds queries like
  ```sql
  SELECT CAST(5, 'UInt8') as testSample;
  ```
  via <b>ToQueryPart</b> type class for every supported type
- Executes *select*
- Parses the result
- Checks if result equals initial value

---

```haskell
import T2WriteReadEquality (t2)
```

1. Runs *insertInto* of a sample into the all supported types table
2. Runs *selectFrom* from the same table
3. Checks if result equals sample value

---

```haskell
import T3Multithreading (t3)
```


Runs 10000 concurrent queries via single connection

---

```haskell
import T4MissmatchErrors (t4)
```

<div>
  <p>
    1. Runs queries with types and names missmatch and handles error
  </p>
  <p>
    You can manually run database and tests:
  </p>
</div>

---

```haskell
import T5Settings (t5)
```

Main function

```haskell
import ClickHaskell (openConnection, defaultConnectionArgs, overrideMaxRevision)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  connOld <- openConnection (overrideMaxRevision 1 defaultConnectionArgs)
  t1 connection; t1 connOld
  t2 connection; t2 connOld
  t3 connection; t3 connOld
  t4 connection; t4 connOld
  t5 connection; t5 connOld
```

