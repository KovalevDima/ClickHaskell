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
              builtins.match ".*<pre><code class=\"sql\" data-lang=\"sql\"\n>(.*);\n</code></pre>.*"
              (builtins.readFile path)
            )
          );
        supportedGHCs = [ "ghc8107" "ghc902" "ghc926" "ghc948" "ghc966" "ghc984" "ghc9101"];
        schemas = [
          (extractSqlFromMarkdown ./usage/api/insertInto/index.lhs)
          (extractSqlFromMarkdown ./usage/api/selectFromView/index.lhs)
          (extractSqlFromMarkdown ./testing/T2WriteReadEquality.hs)
          (extractSqlFromMarkdown ./usage/modules/visits/ChVisits.hs)
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
              "test-${ghc}-${app}" = import ./contribution/testing.nix {
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
            (ghc: {"${ghc}" = import ./contribution/project.nix {inherit pkgs ghc;};})
            supportedGHCs
        );
        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [config.haskellProjects.ghc966.outputs.devShell];
            packages = with pkgs; with haskellPackages; with (self'.packages);
              [clickhouse nil eventlog2html graphmod ghc966-html2hs];
          };
          ghc8107 = pkgs.mkShell {
            inputsFrom = [];
            packages = with pkgs; with haskellPackages; with (self'.packages);
              [clickhouse nil eventlog2html graphmod ghc8107-html2hs haskell.compiler.ghc8107 cabal-install];
          };
        };
        # Build documnetation
        packages = {
          "documentation" = import ./contribution/documentation.nix {inherit pkgs;};
          "ClickHaskell-dist" = import ./contribution/hackage.nix {
            inherit pkgs;
            distPackage = self'.packages.ghc966-ClickHaskell;
          };
        };
      };
    }
    //
    {
      nixosModules = {
        default = import ./contribution/systemModule.nix self;
      };
      hydraJobs = { inherit (self) packages; };
    };
}
