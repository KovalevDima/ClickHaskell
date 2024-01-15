{
  description = "ClickHaskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {self', ...}: {
        haskellProjects.default = {};
        packages.default = self'.packages.ClickHaskell-client;
        packages.default = self'.packages.ClickHaskell-generics;
        packages.default = self'.packages.ClickHaskell-tables;
        packages.default = self'.packages.ClickHouse-db-types;
      };
    };
}
