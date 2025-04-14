# 1.0.0 -- ?

## Features:
- Support and CI for GHCs: `8.10.7`, `9.0.2`, `9.2.6`
- ~20% optimization of time and alloc (perf test 1 benchmark)

# 0.2.0 -- 23-03-2023

ClickHaskell documentation got it's own domain name: https://clickhaskell.dev/


## Fixes:
- Improved multithreading connection usage (+ added test)
- Unexpected behavior on expected and result column type mismatches (+ added test)

## Features:
- Additional GHC versions tests: `9.4.8`, `9.8.4`, `9.10.1`
- Query serialization support for UUID (+ added test)
- Export of client errors for exception handling
- Dropped vector dependency
- Introduced memory consumption test (64M limit) on parallel reading and writing of 1 million rows
- Added new reading wrapper for generateRandom function
- Depricating `Ch*` prefixes on types:
  - `ChUInt*` -> `UInt*` (type synonyms to Word*)
  - `ChInt*` -> `Int*` (reexport of Data.Int)
  - `ChDate` -> `Date` (ClickHaskell type)
  - `ChDateTime` -> `DateTime` (ClickHaskell type)
  - `ChArray` -> `Array` (ClickHaskell type)
  - `ChUUID` -> `UUID` (ClickHaskell type)
- openNativeConnection now passes $HOME and $USERNAME variables to query info

## Breaking changes:
- ### New UserErrors on types and columns names missmatches
  This change helps protect a user from unexpected behavior
  `UnmatchedType` error occurs when the expected type doesn't match the resulting one
  `UnmatchedColumn` error occurs when the expected column name doesn't match the resulting one
  ```haskell
  data ExpectedName = MkExpectedName
    { expectedName :: ChInt64
    }
    deriving (Generic)
    deriving anyclass (ReadableFrom (Columns ExpectedColumns))

  type ExpectedColumns = '[ Column "expectedName" ChInt64]

  -- Will throw UnmatchedColumn
  void $
    select
        @ExpectedColumns @ExpectedName connection
        (toChType "SELECT * FROM generateRandom('unexpectedName Int64', 1, 10, 2) LIMIT 1")
        pure

  -- Will throw UnmatchedType
  void $
    select
      @ExpectedColumns @ExpectedName connection
      "SELECT * FROM generateRandom('expectedName String', 1, 10, 2) LIMIT 1"
      pure
  ```

- ### Migration to streaming API
    The result of selects now exposes the block by block handling result. So you need to pass the handler and to process the list of results
    ```haskell
    result <-
      sum <$>
        select
          @ExampleColumns
          @ExampleData
          connection "\
          \SELECT * \
          \FROM generateRandom('a1 Int64', 1, 10, 2) \
          \LIMIT 1_000_00 \
          \ "
    ```
    now looks like
    ```haskell
    result <-
      sum <$>
        select
          @ExampleColumns
          @ExampleData
          connection "\
          \SELECT * \
          \FROM generateRandom('a1 Int64', 1, 10, 2) \
          \LIMIT 1_000_00 \
          \ "
          (pure . sum)
    ```

- ### DateTime type now parametrized with timezone
    Every DateTime type annotations
    ```haskell
    type A = ChDateTime -- DateTime
    type B = ChDateTime -- DateTime('UTC')
    ```
    should be changed with
    ```haskell
    type A = ChDateTime ""    -- DateTime
    type B = ChDateTime "UTC" -- DateTime('UTC')
    ```

- ### Migration to single module distribution
    You need to move all imports such as
    ```haskell
    import ClickHaskell.DbTypes (ChInt8)
    ```
    to ClickHaskell module
    ```haskell
    import ClickHaskell (ChInt8)
    ```

# 0.1.0 -- 04-12-2024

Initial release
