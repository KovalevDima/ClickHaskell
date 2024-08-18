# ClickHaskell 
[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)

ClickHaskell is a `set of Haskell libraries` and `(WIP)clickhouse-lock CLI` \
which allows you to build reliable integrations with [ClickHouse](https://clickhouse.com/) DBMS

Library API allows you to:

1. (WIP) naturally `validate backward compatability` \
between application and DBMS on CI/CD

2. `avoid manual testing` of queries-parsers matching

3. `generate encoders/parsers` transparently

4. `generate data mappers` on reading/writing

5. `avoid writing SQL` without any DSL

Library API restricts you by:

1. relying on `using fixed` `Table`s/`Table function`s \
as read/write models contracts

2. relying on `database first` development approach

3. benefiting via usage of `CQRS pattern`

# Interesting

[Library documentation](https://getshoptv.github.io/ClickHaskell/)

[ClickHouse documentation](https://clickhouse.com/docs)

[Wikipedia article about CQRS](https://en.wikipedia.org/wiki/Command%E2%80%93query_separation)

[Development](./development.md)
