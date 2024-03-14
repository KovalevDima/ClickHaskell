CREATE TABLE example
(
    `a1` Int64,
    `a2` LowCardinality(String),
    `a3` DateTime,
    `a4` UUID,
    `a5` Int64,
    `a6` LowCardinality(Nullable(String)),
    `a7` LowCardinality(String)
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ()
