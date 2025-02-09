{
  description = "ClickHaskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
  };

  outputs = inputs @ {self, flake-parts, nixpkgs, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.process-compose-flake.flakeModule
      ];
      perSystem = {self', pkgs, config, lib, ...}:
      let
        wrapDefaultClickHouse = inputSchemas: {
          enable = true;
          extraConfig.http_port = 8123;
          initialDatabases = [ {name="default"; schemas=inputSchemas;} ];
        };
        extractSqlFromMarkdown = path:
          builtins.toFile (builtins.baseNameOf path) (
            lib.strings.concatStrings (
              builtins.match ".*```sql\n(.*);\n```.*"
              (builtins.readFile path)
            )
          );
      in
      {
        process-compose = {
          default = {
            imports = [inputs.services-flake.processComposeModules.default];
            services.clickhouse."dev-database" = wrapDefaultClickHouse [
              (extractSqlFromMarkdown ./usage/insertInto.lhs)
              (extractSqlFromMarkdown ./usage/selectFromView.lhs)
              (extractSqlFromMarkdown ./testing/PT1Simple.hs)
              (extractSqlFromMarkdown ./testing/T2WriteReadEquality.hs)
            ];
          };
          "testing" = {
            imports = [inputs.services-flake.processComposeModules.default];
            settings.processes.integration-test = {
              command = "${self'.apps.tests.program}";
              availability.exit_on_end = true;
              depends_on.testing-db.condition = "process_healthy";
            };
            services.clickhouse."testing-db" = wrapDefaultClickHouse [
              (extractSqlFromMarkdown ./testing/T2WriteReadEquality.hs)
            ];
          };
          "profiling" = import ./testing/performance.nix {
            inherit pkgs inputs;
            app = self'.apps."prof-simple"; 
            schemas = [(extractSqlFromMarkdown ./testing/PT1Simple.hs)];
          };
          "one-billion-streaming" = import ./testing/performance.nix {
            inherit pkgs inputs;
            app = self'.apps."prof-1bil-stream";
          };
        };
        # ClickHaskell project itself with Haskell env
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          settings = {
            ClickHaskell = {libraryProfiling = true; haddock = true;};
            tests        = {libraryProfiling = true; executableProfiling = true;};
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = with pkgs; with haskellPackages;
            [clickhouse nixfmt nil eventlog2html graphmod cabal-plan];
        };
        # Build documnetation
        packages = {
          "documentation" = import ./contribution/documentation.nix {
            inherit pkgs;
            compiler = lib.getExe' self'.packages.contribution "documentation-compiler";
          };
          "ClickHaskell-dist" = import ./hackage.nix {
            inherit pkgs;
            distPackage = self'.packages.ClickHaskell;
          };
        };
      };
    };
}
