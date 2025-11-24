# 1.0.0 -- ?

## Features:
- Support and CI for GHCs: `8.10.7`, `9.0.2`, `9.2.8`, `9.12.2`
- ~50% optimization of time and alloc (perf test 1 benchmark)
- Added function `command` for statements with no result
- Added `DateTime64`, `UInt256`, `Enum8`, `Enum16`, `Array(T)`(partial) types support
- Added experimental support for settings passing (watch `addSetting` fuction)

## Fixes:
- Fixed unexpected behavior when the number of result columns was different from expected.
  A `UserError` exception `UnmatchedColumnsCount` is now raised in such cases
- Query serialization for UInt128 bigger than 999999999999999934463

## Breaking changes:
- ### select/insert API changes
  - `insert` and `select` no longer accept raw queries
  - The following functions were removed:
    `selectFrom`, `selectFromView`, `generateRandom`, `insertInto`.
  - Now you should use statement generators instead:
    - `select` with `fromTable`, `fromView`, `fromGenerateRandom`, `unsafeMkSelect`
    - `insert` with `intoTable`, `unsafeMkInsert`
- ### Generic API changes
  `ReadableFrom` and `WritableInto` was replaced with `ClickHaskell`\
  Now you should declare single instance for every API parts
  ```haskell
  data MyData =
    ...
    deriving (Generic)
    deriving anyclass (ClickHaskell ExampleColumns)
  ```
  ```haskell
  data MyData =
    ...
    deriving (Generic)
    deriving anyclass
      ( ReadableFrom (Columns ExampleColumns)
      , WritableInto (Table "profiler" ExampleColumns)
      )
  ```
- ### `Serializable` type class was unexported
- ### `DeserializableColumn` type class was renamed to `SerializableColumn`
- ### `IsChType` instance changes
  `ToChType` type family was deleted
- ### `FromChType` typeclass merged into `ToChType`
- ### `parameter` function now doesn't apply toChType
- ### Connection initialization API changes
  1. `ChCredential` renamed to `ConnectionArgs`
  2. `defaultCredentials` renamed to `defaultConnectionArgs`
  3. `openNativeConnection` renamed to `openConnection`
  4. `ConnectionArgs` constructor now are not exported

     You need to use new modifiers:\
     `setHost`, `setPort`, `setUser`, `setDatabase`, `setPassword`

     Connection initialization example:
     ```haskell
     initMyConnection :: IO Connection
     initMyConnection = do
       connection <-
         openConnection
           . setUser "default"
           . setPassword ""
           . setDatabase "default"
           . setPort "9000"
           . setHost "localhost"
           $ defaultConnectionArgs
       pure connection
     ```
- ### `client_name` format was changed
  from `ClickHaskell-x.y.z` to `ClickHaskell`

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
