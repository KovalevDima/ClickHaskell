{
  description = "ClickHaskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
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
        config,
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
        extractSqlFromMarkdown = path:
          builtins.toFile (builtins.baseNameOf path) (
            lib.strings.concatStrings (
              builtins.match ".*```sql\n(.*);\n```.*"
              (builtins.readFile path)
            )
          );
        extractDist = pkg: "${pkgs.haskell.lib.sdistTarball pkg}/${pkg.name}.tar.gz";
        extractDocs = pkg: "${pkgs.haskell.lib.documentationTarball pkg}/${pkg.name}-docs.tar.gz";
      in {
        # Database wrapper with all schemas initialization
        process-compose."default" = {
          imports = [inputs.services-flake.processComposeModules.default];
          services.clickhouse."dev-database" = wrapDefaultClickHouse [
            (extractSqlFromMarkdown ./documentation/parametrized-view/README.lhs)
            (extractSqlFromMarkdown ./documentation/writing/Writing.lhs)
            ./integration-testing/clickhouse/writeReadEquality.sql
          ];
        };
        # Integration testing wrapper
        process-compose."integration-testing" = {
          imports = [inputs.services-flake.processComposeModules.default];
          cli.up.tui = false; # GitHub Actions doesn't work with TUI. Don't enable it
          settings.processes.integration-test = {
            command = "${self'.apps.integration-tests.program}";
            availability.exit_on_end = true;
            depends_on.integration-testing-db.condition = "process_healthy";
          };
          services.clickhouse."integration-testing-db" = wrapDefaultClickHouse [
            ./integration-testing/clickhouse/writeReadEquality.sql
          ];
        };
        # Profiling wrapper
        process-compose."profiling" = let
          programName = "profiler";
        in {
          imports = [inputs.services-flake.processComposeModules.default];
          services.clickhouse."profiler-db" = wrapDefaultClickHouse [
            (extractSqlFromMarkdown ./documentation/writing/Writing.lhs)
          ];
          settings.processes.profiling = {
            command = "${self'.apps.${programName}.program}";
            depends_on.profiler-db.condition = "process_healthy";
          };
          settings.processes.dump-artifacts = {
            command = "${lib.getExe' pkgs.haskellPackages.eventlog2html "eventlog2html"} ./${programName}.eventlog";
            # availability.exit_on_end = true;
            depends_on.profiling.condition = "process_completed_successfully";
          };
        };
        process-compose."profiling2" = let
          programName = "profiler2";
        in {
          imports = [inputs.services-flake.processComposeModules.default];
          services.clickhouse."profiler-db" = wrapDefaultClickHouse [
            (extractSqlFromMarkdown ./documentation/writing/Writing.lhs)
          ];
          settings.processes.profiling = {
            command = "${self'.apps.${programName}.program}";
            depends_on.profiler-db.condition = "process_healthy";
          };
          settings.processes.dump-artifacts = {
            command = "${lib.getExe' pkgs.haskellPackages.eventlog2html "eventlog2html"} ./${programName}.eventlog";
            # availability.exit_on_end = true;
            depends_on.profiling.condition = "process_completed_successfully";
          };
        };
        # ClickHaskell project itself with Haskell env
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          settings = {
            ClickHaskell = {
              libraryProfiling = true;
              haddock = true;
            };
            ClickHaskell-http-client = {
              libraryProfiling = true;
              haddock = true;
            };
            profilers = {
              executableProfiling = true;
              libraryProfiling = true;
            };
          };
          devShell.tools = hp: {
            inherit (hp) eventlog2html graphmod;
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          packages = [
            pkgs.clickhouse
          ];
        };
        # Build documnetation
        packages."documentation" = pkgs.stdenv.mkDerivation {
          name = "documentation";
          buildInputs = [];
          src = pkgs.nix-gitignore.gitignoreSourcePure [] ./.;

          buildPhase = ''
            ${lib.getExe' self'.packages.ClickHaskell-documentation "ClickHaskell-documentation"} build --verbose
          '';

          installPhase = ''
            mkdir -p "$out"
            cp -r ./_site "$out"
          '';
        };
        packages."ClickHaskell-dist" =
          pkgs.runCommand "ClickHaskell-dist" {} ''
            mkdir $out
            mkdir -m 777 $out/packages $out/docs
            cp -r ${extractDist self'.packages.ClickHaskell} $out/packages
            cp -r ${extractDocs self'.packages.ClickHaskell} $out/docs
        '';
        packages."ClickHaskell-http-client-dist" =
          pkgs.runCommand "ClickHaskell-http-client-dist" {} ''
            mkdir $out
            mkdir -m 777 $out/packages $out/docs
            cp -r ${extractDist self'.packages.ClickHaskell-http-client} $out/packages
            cp -r ${extractDocs self'.packages.ClickHaskell-http-client} $out/docs
        '';
      };
    };
}
