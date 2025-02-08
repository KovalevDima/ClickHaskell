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
        dbAndExecutable = {programName, schemas}: {
          imports = [inputs.services-flake.processComposeModules.default];
          services.clickhouse."${programName}-db" = wrapDefaultClickHouse schemas;
          settings.processes = {
            ${programName} = {
              command = "${self'.apps.${programName}.program}";
              depends_on."${programName}-db".condition = "process_healthy";
            };
            dump-artifacts = {
              command = "
                ${lib.getExe' pkgs.haskellPackages.eventlog2html "eventlog2html"} ./${programName}.eventlog
                rm ./${programName}.eventlog
                rm ./${programName}.hp
              ";
              availability.exit_on_end = true;
              depends_on.${programName}.condition = "process_completed_successfully";
            };
          };
        };
      in {
        # Database wrapper with all schemas initialization
        process-compose."default" = {
          imports = [inputs.services-flake.processComposeModules.default];
          services.clickhouse."dev-database" = wrapDefaultClickHouse [
            (extractSqlFromMarkdown ./usage/insertInto.lhs)
            (extractSqlFromMarkdown ./usage/selectFromView.lhs)
            (extractSqlFromMarkdown ./testing/PT1Simple.hs)
            (extractSqlFromMarkdown ./testing/T2WriteReadEquality.hs)
          ];
        };
        # Testing wrapper
        process-compose."testing" = {
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
        # Profiling wrapper
        process-compose."profiling" = dbAndExecutable {
          programName = "prof-simple"; 
          schemas = [(extractSqlFromMarkdown ./testing/PT1Simple.hs)];
        };
        process-compose."one-billion-streaming" = dbAndExecutable {
          programName = "prof-1bil-stream";
          schemas = [];
        };
        # ClickHaskell project itself with Haskell env
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          settings = {
            ClickHaskell = {libraryProfiling = true; haddock = true;};
            tests        = {libraryProfiling = true; executableProfiling = true;};
          };
          devShell.tools = hp: {
            inherit (hp) eventlog2html graphmod cabal-plan;
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = [pkgs.clickhouse pkgs.nixfmt];
        };
        # Build documnetation
        packages."documentation" = pkgs.stdenv.mkDerivation {
          name = "documentation";
          src = pkgs.nix-gitignore.gitignoreSourcePure [] ./.;

          buildPhase = ''
            ${lib.getExe' self'.packages.contribution "documentation-compiler"} build --verbose
          '';

          installPhase = ''
            mkdir -p "$out"
            cp -r ./_site "$out"
          '';
        };
        packages."ClickHaskell-dist" =
          with pkgs.haskell.lib;
          with self'.packages;
          pkgs.runCommand "ClickHaskell-dist" {} ''
            mkdir $out
            mkdir -m 777 $out/packages $out/docs
            cp -r ${sdistTarball ClickHaskell}/${ClickHaskell.name}.tar.gz $out/packages
            cp -r ${documentationTarball ClickHaskell}/${ClickHaskell.name}-docs.tar.gz $out/docs
          '';
      };
    };
}
