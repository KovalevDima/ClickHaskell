{
  description = "ClickHaskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
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
        inputs.process-compose-flake.flakeModule
      ];
      perSystem = {self', pkgs, lib, ...}: {
        process-compose."default" = {
          imports =  [
            inputs.services-flake.processComposeModules.default
          ];
          services.clickhouse."dev-database" = {
            enable=true;
            extraConfig = {
              http_port = 8123;
            };
            initialDatabases = [
              { name = "example";
                schemas = [
                  ./dev/clickhouse/example.sql
                ];
              }
            ];
          };
        };
        haskellProjects.default = {
          autoWire = [ "packages" ];
        };
    };
  };
}
