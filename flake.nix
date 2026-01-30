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

        # process-compose runners
        mkProfRunners =
          ({ghc, app}: {
              "test-${ghc}-${app}" = import ./contribution/prof.nix {
                inherit pkgs inputs;
                app = self'.apps."${ghc}-${app}";
              };
            }
          );
        profilingRunners = mapMergeAttrsList mkProfRunners
          (lib.cartesianProduct {
            ghc = supportedGHCs; 
            app = ["prof-1bil-stream" "prof-simple" "prof-pings"];
          });
        mkTestRunners =
          (ghc: {
              "test-${ghc}-tests" = import ./contribution/testing.nix {
                inherit pkgs inputs;
                apps = [
                  self'.apps."${ghc}-t001-query-serializaton"
                  self'.apps."${ghc}-t002-rw-equality"
                  self'.apps."${ghc}-t003-multithreading"
                  self'.apps."${ghc}-t004-errors"
                  self'.apps."${ghc}-t005-settings"
                ];
              };
            }
          );
        testRunners = mapMergeAttrsList mkTestRunners supportedGHCs;
        devEnv = {
          default =
            import ./contribution/localServer.nix {
              inherit inputs pkgs;
              app = self'.apps.ghc9103-server;
              agent = self'.apps.ghc9103-eventlog-agent;
              docDirPath = self'.packages."documentation";
            };
          };

        # Development shells
        mkHaskellShell =
          (ghc: {"dev-${ghc}" = pkgs.mkShell {
            inputsFrom =
              if ghc == "ghc9103"
              then [config.haskellProjects.${ghc}.outputs.devShell]
              else [];
            packages = with pkgs; with haskellPackages;
              [ clickhouse nodejs pnpm nil eventlog2html graphmod markdown-unlit cloc
              ] ++ (if ghc == "ghc9103" then [] else [haskell.compiler."${ghc}"]) ;
            };
          });
        extraShells = mapMergeAttrsList mkHaskellShell supportedGHCs;
        defaultShell = {default = extraShells.dev-ghc9103;};

        # GHC env
        mkProject = ghc: {"${ghc}" = import ./contribution/project.nix {inherit pkgs ghc inputs;};};
        projects = mapMergeAttrsList mkProject supportedGHCs;
        staticBinariesProject = {
          "static" =
            import ./contribution/project.nix {
              inherit pkgs inputs;
              ghc = "ghc9103";
              isStatic = true;
            };
          };
      in
      {
        process-compose = devEnv // profilingRunners // testRunners;
        haskellProjects = projects // staticBinariesProject;
        devShells = defaultShell // extraShells;
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
