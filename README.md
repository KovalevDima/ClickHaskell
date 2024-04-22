# ClickHaskell

ClickHaskell is a set of Haskell libraries which allows you to \
build integrations with [ClickHouse](https://clickhouse.com/) DBMS

ClickHaskell aims to be balanced between lightweightness, composability and simplicity

# Design factors

1. ClickHouse stores data in a [tables](https://clickhouse.com/docs/en/guides/creating-tables)
2. ClickHouse has [HTTP interface](https://clickhouse.com/docs/en/interfaces/http) for accessing and manipulating `tables` states
3. HTTP request [message-body](https://datatracker.ietf.org/doc/html/rfc2616#section-4.3) to a DBMS server consists of a [statement](https://clickhouse.com/docs/en/sql-reference/statements)
4. There are a few [format](https://clickhouse.com/docs/en/interfaces/formats)s that defines data serialization in `HTTP message body` of request/response 
5. ClickHaskell should cover only problems related to communication protocol of ClickHouse DBMS 
6. ClickHaskell should provide safe API that will help the end user think less about the ClickHouse interface

# Development environment

We are using [Nix flakes](https://nixos.wiki/wiki/Flakes) to set up whole development environment

When you have configured Nix. Enter into [nix shell](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-develop) and then run
```bash
nix run
```

Nix will setup a database with development required schemas for you.

After `dev-database` Health status is `Ready`, you can start to work with database dependent code parts

# Profiling

Setup [development environment](#development-environment) and then run

```bash
cabal run profiling  --enable-profiling --ghc-options=-fprof-late
```

Cabal will build binary and start the profiling process

After end of profiling you can visualize the result by running

```bash
eventlog2html ./profiling.eventlog
```
