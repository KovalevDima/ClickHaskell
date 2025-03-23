{ pkgs
, ghc ? "ghc966"
}:
{
  autoWire = ["packages" "apps"];
  basePackages = pkgs.haskell.packages.${ghc};
  settings = {
    ClickHaskell = {libraryProfiling = true; haddock = true;};
    tests        = {libraryProfiling = true; executableProfiling = true;};
  };
}
