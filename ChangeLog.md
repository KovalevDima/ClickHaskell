# 0.2.0 -- yyyy-mm-dd

ClickHaskell documentation got it's own domain name: https://clickhaskell.dev/


## Fixes:
- Improved multithreading connection usage (+ add test)

## Features:
- Query serialization support for UUID (+ add test)
- Export of client errors for exception handling
- Dropped vector dependency
- Introduced memory consumption test (64M limit) on parallel reading and writing of 1 million rows

## API changes:
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
