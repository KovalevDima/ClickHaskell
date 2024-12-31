---
title: Routines
---

## Use `cabal` to work with Haskell sources

1. #### Build everything
    
    ```
    cabal build all
    ```
2. #### Build library only
    
    ```
    cabal build ClickHaskell
    ```
3. #### Run tests

    ```
    cabal run test
    ```
    Note that: test requires database

## Use `nix` to run routine actions

1. #### Initialize database
   to work with database dependent code

    ```
    nix run
    ```
    (delete `./data` dir to reinitialize database)
2. #### Initialize database and run tests

    ```
    nix run .#testing
    ```
    (delete `./data` dir to reinitialize database)
