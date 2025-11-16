
# Basic queries
## By time
```sql
WITH
    addDate(startTime, INTERVAL toDateTime(evTime) SECOND) as t
SELECT t, evType
FROM haskell_eventlog
limit 50
```

## Cap and GC Stats
```sql
SELECT
    evType AS event,
    groupUniqArray(cap) AS caps,
    groupUniqArray(heapCapset) AS capsets,
    count() AS total
FROM haskell_eventlog
GROUP BY event
ORDER BY total ASC
```



# Data compression analysis

## Size by columns
```sql
WITH
    data_compressed_bytes AS size_comp,
    data_uncompressed_bytes AS size_uncomp
SELECT
    `table`,
    name,
    formatReadableSize(size_comp) AS compressed,
    formatReadableSize(size_uncomp) AS uncompressed,
    round(size_uncomp / size_comp, 2) AS ration,
    compression_codec as codec
FROM system.columns
WHERE `table` like 'haskell_eventlog%'
ORDER BY name, table
```

## Size by tables
```sql
WITH
    sumIf(bytes_on_disk, active) as size_comp,
    sumIf(data_uncompressed_bytes, active) as size_uncomp
SELECT
    table,
    formatReadableSize(size_comp) AS compressed,
    formatReadableSize(size_uncomp) AS uncompressed,
    round(size_uncomp/size_comp, 2) as ratio,
    sumIf(rows, active) AS rows
FROM system.parts
WHERE `table` LIKE 'haskell_eventlog%'
GROUP BY `table`
```
