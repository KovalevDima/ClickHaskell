{
  description = "ClickHaskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
    clickhouse = {
      url = "github:ClickHouse/ClickHouse";
      flake = false;
    };
    wide-word = {
      url = "github:erikd/wide-word?commit=cd4b82c1921826d9f63ef80dae6c9003ba587b73";
      flake = false;
    };
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
        mapMergeAttrsList = f: x: lib.mergeAttrsList (map f x);
        supportedGHCs = ["ghc948" "ghc967" "ghc984" "ghc9103" "ghc9122"];
      in
      {
        process-compose = {
          default = import ./contribution/localServer.nix {
            inherit inputs pkgs;
            app = self'.apps.ghc9103-server;
            agent = self'.apps.ghc9103-eventlog-agent;
            docDirPath = self'.packages."documentation";
          };
        }
        //
        mapMergeAttrsList
          ({ghc, app}: {
              "test-${ghc}-${app}" = import ./contribution/testing.nix {
                inherit pkgs inputs;
                app = self'.apps."${ghc}-${app}";
              };
            }
          )
          (lib.cartesianProduct {
            ghc = supportedGHCs; 
            app = ["prof-1bil-stream" "prof-simple" "tests" "prof-pings"];
          });
        haskellProjects =
          let
            mkProject = ghc: {"${ghc}" = import ./contribution/project.nix {inherit pkgs ghc inputs;};};
            static = {
              "static" = import ./contribution/project.nix {
                inherit pkgs inputs;
                ghc = "ghc9103";
                isStatic = true;
              };
            };
          in mapMergeAttrsList mkProject supportedGHCs // static;
        devShells =
          mapMergeAttrsList
            (ghc: {"dev-${ghc}" = pkgs.mkShell {
              inputsFrom = [];
              packages = with pkgs; with haskellPackages; with (self'.packages);
                [ clickhouse nil eventlog2html graphmod nodejs
                  haskell.compiler."${ghc}" cabal-install
                ];
              };
            })
            supportedGHCs
          //
          {
            default = pkgs.mkShell {
              inputsFrom = [config.haskellProjects.ghc9103.outputs.devShell];
              packages = with pkgs; with haskellPackages; with (self'.packages);
                [clickhouse nodejs pnpm nil eventlog2html graphmod markdown-unlit cloc];
            };
          };
        # Build documnetation
        packages = {
          "documentation" = import ./documentation/documentation.nix {inherit pkgs;};
          "ClickHaskell-dist" = import ./contribution/hackage.nix {
            inherit pkgs;
            distPackage = self'.packages.ghc9103-ClickHaskell;
          };
          "ClickHaskell-tls-dist" = import ./contribution/hackage.nix {
            inherit pkgs;
            distPackage = self'.packages.ghc9103-ClickHaskell-tls;
          };
          "settsFile" = import ./contribution/genSettings.nix {
            inherit pkgs;
            clickhouseRepo = inputs.clickhouse;
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
