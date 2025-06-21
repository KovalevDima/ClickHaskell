{
  description = "ClickHaskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
    # tmp override
    wide-word = {
      url = "github:erikd/wide-word";
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
        supportedGHCs = ["ghc8107" "ghc902" "ghc928" "ghc948" "ghc966" "ghc984" "ghc9101" "ghc9122"];
      in
      {
        process-compose = {
          default = import ./contribution/localServer.nix {
            inherit inputs pkgs;
            app = self'.apps.ghc984-server;
            agent = self'.apps.ghc984-eventlog-agent;
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
            app = ["prof-1bil-stream" "prof-simple" "tests"];
          });
        haskellProjects =
          mapMergeAttrsList
            (ghc: {"${ghc}" = import ./contribution/project.nix {inherit pkgs ghc inputs;};})
            supportedGHCs;
        devShells =
          mapMergeAttrsList
            (ghc: {"dev-${ghc}" = pkgs.mkShell {
              inputsFrom = [];
              packages = with pkgs; with haskellPackages; with (self'.packages);
                [ clickhouse nil eventlog2html graphmod
                  self'.packages."${ghc}-html2hs" haskell.compiler."${ghc}" cabal-install
                ];
              };
            })
            supportedGHCs
          //
          {
            default = pkgs.mkShell {
              inputsFrom = [config.haskellProjects.ghc9122.outputs.devShell];
              packages = with pkgs; with haskellPackages; with (self'.packages);
                [clickhouse nil eventlog2html graphmod ghc9122-html2hs];
            };
          };
        # Build documnetation
        packages = {
          "documentation" = import ./contribution/documentation.nix {inherit pkgs;};
          "ClickHaskell-dist" = import ./contribution/hackage.nix {
            inherit pkgs;
            distPackage = self'.packages.ghc984-ClickHaskell;
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
