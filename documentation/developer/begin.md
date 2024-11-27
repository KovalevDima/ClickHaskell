---
title: Begin
---

ClickHaskell contributors using `Nix` package manager to setup everything we need for development without Docker

### Why Nix

1. Nix allows us to have reproducable enviroment
2. Nix allows to setup everything you need for end-to-end development
3. Nix has a lot of usefull tools to automate daily routines


# How to start with Nix

### Install Nix

Nix supported on:

1. Windows Subsystem for Linux
2. Linux
3. MacOS

You can install it with flakes enabled via [DeterminateSystems/nix-installer](https://github.com/DeterminateSystems/nix-installer)

Or you can install it via [official installer](https://nixos.org/download/) and [enable flakes yourself](https://nixos.wiki/wiki/Flakes)

### Setup development environment

After you have Nix installed you can:
```sh
nix develop
```
to manually enter shell with provided

1. Cabal
2. GHC
3. Haskell Language Server
4. clickhouse-client

You can also setup [direnv]() to automatically enter the shell \
when you enter the project directory in `terminal` or `VSCode` ([via extention](https://marketplace.visualstudio.com/items?itemName=mkhl.direnv))

# What next?
Read [routines](/documentation/developer/routines.html)
