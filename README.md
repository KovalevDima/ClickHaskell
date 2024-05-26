# ClickHaskell

[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)

ClickHaskell is a set of Haskell libraries which allows\
you to build integrations with [ClickHouse](https://clickhouse.com/) DBMS

ClickHaskell aims to be balanced between\
lightweightness, composability and simplicity

# Design factors

1. ClickHouse stores data in a [tables](https://clickhouse.com/docs/en/guides/creating-tables)
2. ClickHouse has [HTTP interface](https://clickhouse.com/docs/en/interfaces/http) for accessing and manipulating `tables` states
3. HTTP request [message-body](https://datatracker.ietf.org/doc/html/rfc2616#section-4.3) to a DBMS server consists of a [statement](https://clickhouse.com/docs/en/sql-reference/statements)
4. There are a few [format](https://clickhouse.com/docs/en/interfaces/formats)s that defines data serialization in `statement`
5. ClickHaskell should cover only problems related to communication protocol of ClickHouse DBMS 
6. ClickHaskell should provide safe API that will help the end user think less about ClickHouse interface

# Development environment

We are using [Nix flakes](https://nixos.wiki/wiki/Flakes) to set up whole development environment.

When you have configured Nix. Enter into [nix shell](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-develop) and then run:
```bash
nix run
```

Nix will setup a database with development required schemas for you.

After `dev-database` Health status is `Ready`, you can start to work with database dependent code parts.

# Profiling

```bash
nix run .#profiling
```

will start profiling process.

You should see `profiler.eventlog.html` in the project \
directory after the profiling process has completed successfully
