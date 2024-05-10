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
      perSystem = {
        self',
        pkgs,
        lib,
        ...
      }: {
        process-compose."default" = {
          imports = [
            inputs.services-flake.processComposeModules.default
          ];
          services.clickhouse."dev-database" = {
            enable = true;
            extraConfig = {
              http_port = 8123;
            };
            initialDatabases = [
              {
                name = "example";
                schemas = [
                  ./dev/clickhouse/example.sql
                ];
              }
            ];
          };
        };
        process-compose."integration-testing" = {
          imports = [
            inputs.services-flake.processComposeModules.default
          ];
          tui = false;
          settings.processes.integration-test = {
            command = "${self'.apps.integration-tests.program}";
            depends_on.integration-testing-db.condition = "process_healthy";
            availability = {
              exit_on_end = true;
            };
          };
          services.clickhouse."integration-testing-db" = {
            enable = true;
            extraConfig = {
              http_port = 8123;
            };
            initialDatabases = [
              {
                name = "default";
                schemas = [
                ];
              }
            ];
          };
        };
        haskellProjects.default = {
          autoWire = ["packages" "devShells" "apps"];
          devShell.tools = hp: {
            inherit (hp) eventlog2html;
          };
        };
      };
    };
}
