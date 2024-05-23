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
      }: let
        wrapDefaultClickHouse = inputSchemas: {
          enable = true;
          extraConfig.http_port = 8123;
          initialDatabases = [
            {
              name = "default";
              schemas = inputSchemas;
            }
          ];
        };
      in {
        process-compose."default" = {
          imports = [inputs.services-flake.processComposeModules.default];
          services.clickhouse."dev-database" = wrapDefaultClickHouse [
            ./examples/clickhouse/exampleWriteRead.sql
            ./examples/clickhouse/exampleViewTable.sql
          ];
        };
        process-compose."integration-testing" = {
          imports = [inputs.services-flake.processComposeModules.default];
          tui = false;
          settings.processes.integration-test = {
            command = "${self'.apps.integration-tests.program}";
            availability.exit_on_end = true;
            depends_on.integration-testing-db.condition = "process_healthy";
          };
          services.clickhouse."integration-testing-db" = wrapDefaultClickHouse [
            ./integration-testing/clickhouse/writeReadEquality.sql
          ];
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
