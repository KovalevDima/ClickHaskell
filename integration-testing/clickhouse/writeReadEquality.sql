CREATE TABLE writeReadEqualityTable
(
    `dateTime` DateTime,
    `dateTimeNullable` Nullable(DateTime),
    `int128` Int128,
    `int128Nullable` Nullable(Int128),
    `int16` Int16,
    `int16Nullable` Nullable(Int16),
    `int32` Int32,
    `int32Nullable` Nullable(Int32),
    `int64` Int64,
    `int64Nullable` Nullable (Int64),
    `int8` Int8,
    `int8Nullable` Nullable(Int8),
    `string` String,
    `stringNullable` Nullable(String),
    `uint128` UInt128,
    `uint128Nullable` Nullable(UInt128),
    `uint16` UInt16,
    `uint16Nullable` Nullable (UInt16),
    `uint32` UInt32,
    `uint32Nullable` Nullable(UInt32),
    `uint64` UInt64,
    `uint64Nullable` Nullable(UInt64),
    `uint8` UInt8,
    `uint8Nullable` Nullable(UInt8),
    `uuid` UUID,
    `uuidNullable` Nullable(UUID)
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ()
