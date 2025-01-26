# 0.2.0 -- yyyy-mm-dd

ClickHaskell documentation got it's own domain name: https://clickhaskell.dev/


## Fixes:
- Improved multithreading connection usage (+ add test)

## Features:
- Query serialization support for UUID (+ add test)
- Export of client errors for exception handling

## API changes:
- ### DateTime type now parametrized with timezone
    Migration guide:\
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
    Migration guide: \
    You need to move all imports such as
    ```haskell
    import ClickHaskell.DbTypes (ChInt8)
    ```
    to ClickHaskell module
    ```haskell
    import ClickHaskell (ChInt8)
    ```
