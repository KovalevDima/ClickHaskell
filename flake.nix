{
  description = "ClickHaskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
  };

  outputs = {self, flake-parts, nixpkgs, ...} @ inputs:
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
          "testing" = import ./testing/testing.nix {
            inherit inputs;
            app = self'.apps."ghc966-tests";
            schemas = [(extractSqlFromMarkdown ./testing/T2WriteReadEquality.hs)];
          };
          "profiling" = import ./testing/performance.nix {
            inherit pkgs inputs;
            app = self'.apps."ghc966-prof-simple"; 
            schemas = [(extractSqlFromMarkdown ./testing/PT1Simple.hs)];
          };
          "ghc9101-profiling" = import ./testing/performance.nix {
            inherit pkgs inputs;
            app = self'.apps."ghc9101-prof-simple";
            schemas = [(extractSqlFromMarkdown ./testing/PT1Simple.hs)];
          };
          "one-billion-streaming" = import ./testing/performance.nix {
            inherit pkgs inputs;
            app = self'.apps."prof-1bil-stream";
          };
          "ghc9101-one-billion-streaming" = import ./testing/performance.nix {
            inherit pkgs inputs;
            app = self'.apps."ghc9101-prof-1bil-stream";
          };
        };
        # ClickHaskell project itself with Haskell env
        haskellProjects = {
          "ghc926" = import ./distribution/project.nix {
            inherit pkgs;
            basePackages = pkgs.haskell.packages.ghc926;
          };
          "ghc948" = import ./distribution/project.nix {
            inherit pkgs;
            basePackages = pkgs.haskell.packages.ghc948;
          };
          "ghc966" = import ./distribution/project.nix {
            inherit pkgs;
            basePackages = pkgs.haskell.packages.ghc966;
          };
          "ghc984" = import ./distribution/project.nix {
            inherit pkgs;
            basePackages = pkgs.haskell.packages.ghc984;
          };
          "ghc9101" = import ./distribution/project.nix {
            inherit pkgs;
            basePackages = pkgs.haskell.packages.ghc9101;
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.ghc966.outputs.devShell];
          packages = with pkgs; with haskellPackages;
            [clickhouse nixfmt nil eventlog2html graphmod cabal-plan];
        };
        # Build documnetation
        packages = {
          "documentation" = import ./contribution/documentation.nix {
            inherit pkgs;
            compiler = lib.getExe' self'.packages.ghc966-contribution "documentation-compiler";
          };
          "ClickHaskell-dist" = import ./distribution/hackage.nix {
            inherit pkgs;
            distPackage = self'.packages.ghc966-ClickHaskell;
          };
        };
      };
    }
    //
    {
      nixosModules = {
        default = {config, pkgs, inputs}:  import ./distribution/systemModule.nix {
          inherit config pkgs;
          page = self.packages.${pkgs.system}."documentation";
        };
      };
    };
}
