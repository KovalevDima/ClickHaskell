# Setup Nix

ClickHaskell contributors using `Nix` package manager to setup everything. You can install it via [official installer](https://nixos.org/download/) and [enable flakes](https://nixos.wiki/wiki/Flakes)


After you have Nix installed you can:
```sh
nix develop
```
to manually enter shell with provided: `cabal`, `ghc`, `haskell-laguage-server`, `clickhouse-client`

You can also setup [direnv](https://github.com/nix-community/nix-direnv) to automatically enter the shell

# Run routine actions

#### Start database and documentation server

```
nix run
```

#### Initialize database and run tests

```
nix run .#test-ghc966-tests
```

#### Initilization database and run profiling

```
nix run .#test-ghc966-prof-simple
```

#### Reinitialize database

```
rm -rf ./data
```
(and then restart process-compose)
