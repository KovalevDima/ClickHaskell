{ pkgs
, basePackages ? pkgs.haskell.packages.ghc966
}:
{
  autoWire = ["packages" "apps"];
  inherit basePackages;
  settings = {
    ClickHaskell = {libraryProfiling = true; haddock = true;};
    tests        = {libraryProfiling = true; executableProfiling = true;};
  };
}
