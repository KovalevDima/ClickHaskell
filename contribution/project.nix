{ pkgs
, inputs
, isStatic ? false
, ghc ? "ghc966"
}:
with pkgs.lib;
let
  profiling = {
    libraryProfiling = !isStatic;
    executableProfiling = !isStatic;
  };
  static = {
    staticLibraries = isStatic;
    sharedLibraries = mkIf isStatic false;
    sharedExecutables = mkIf isStatic false;
  };
  haddock = {
    haddock = !isStatic;
  };
in
{
  autoWire = ["packages" "apps"];
  basePackages =
    if isStatic
    then pkgs.pkgsStatic.haskell.packages.${ghc}
    else pkgs.haskell.packages.${ghc};
  settings = {
    ClickHaskell = profiling // static // haddock;
    prof-1bil-stream = profiling // static;
    prof-simple = profiling // static;
    prof-pings = profiling // static;
    vector = {check = mkIf isStatic false;};
  };
  defaults =  {
    devShell.tools = hp: with hp; {
      ghcide = null;
      inherit
        cabal-install
        haskell-language-server;
    };
  };
  packages = {
    # package.source = inputs.package;
    wide-word.source = inputs.wide-word;
  };
}
