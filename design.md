## Design concepts

### Набросок табличного DSL:
```haskell

type EventsTable =
  Table
    "events"
   '[ DefaultColumn "eventId"     ChUUID
    , ColumnExt     "requestTime" ChDateTime        '[DefaultType Default, DefaultExpression "now()"]
    , DefaultColumn "eventType"   (LowCardinality String)
    , ColumnExt     "payload"     (Nullable String) '[DefaultType Default, CodecExpression (ZSTD 1), Comment "Some comment"]
    ]
    MergeTree
    -- | Typecheck is 'PARTITION BY' are empty or a suffix of 'ORDER BY'
    (PARTITION BY '["eventType"])
    (ORDER BY     '["eventType", "eventId"])

```
