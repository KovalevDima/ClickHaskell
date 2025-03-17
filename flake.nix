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
        extractSqlFromMarkdown = path:
          builtins.toFile (builtins.baseNameOf path) (
            lib.strings.concatStrings (
              builtins.match ".*```sql\n(.*);\n```.*"
              (builtins.readFile path)
            )
          );
        supportedGHCs = ["ghc926" "ghc948" "ghc966" "ghc984" "ghc9101"];
        schemas = [
          (extractSqlFromMarkdown ./usage/insertInto.lhs)
          (extractSqlFromMarkdown ./usage/selectFromView.lhs)
          (extractSqlFromMarkdown ./testing/PT1Simple.hs)
          (extractSqlFromMarkdown ./testing/T2WriteReadEquality.hs)
          (extractSqlFromMarkdown ./distribution/server.hs)
          (extractSqlFromMarkdown ./testing/PT1Simple.hs)
          (extractSqlFromMarkdown ./testing/T2WriteReadEquality.hs)
        ];
      in
      {
        process-compose = {
          default = import ./contribution/localServer.nix {
            inherit inputs schemas;
            app = self'.apps.ghc966-server;
            docDirPath = self'.packages."documentation";
          };
        }
        //
        lib.mergeAttrsList (
          map (
            {ghc, app}: {
              "test-${ghc}-${app}" = import ./testing/testing.nix {
                inherit pkgs inputs schemas;
                app = self'.apps."${ghc}-${app}";
              };
            }
          )
          (lib.cartesianProduct {
            ghc = supportedGHCs; 
            app = ["prof-1bil-stream" "prof-simple" "tests"];
          })
        );
        # ClickHaskell project itself with Haskell env
        haskellProjects = lib.mergeAttrsList (
          map
            (ghc: {"${ghc}" = import ./distribution/project.nix {inherit pkgs ghc;};})
            supportedGHCs
        );
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
        default = import ./distribution/systemModule.nix self;
      };
    };
}
