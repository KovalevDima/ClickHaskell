{ pkgs
, inputs
, ghc ? "ghc966"
}:
{
  autoWire = ["packages" "apps"];
  basePackages = pkgs.haskell.packages.${ghc};
  settings = {
    ClickHaskell = {libraryProfiling = true; haddock = true;};
    prof-1bil-stream = {libraryProfiling = true; executableProfiling = true;};
    prof-simple = {libraryProfiling = true; executableProfiling = true;};
  };
  defaults =  {
    devShell.tools = hp: {
      ghcide = null;
    };
  };
  packages = {
    wide-word.source = inputs.wide-word;
  };
}
