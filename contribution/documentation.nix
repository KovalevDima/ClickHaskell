{pkgs}:

pkgs.stdenv.mkDerivation {
  name = "documentation";
  src = pkgs.lib.fileset.toSource {
    root = ../.;
    fileset = ../.;
  };

  dontBuild = true;
  installPhase = ''
    mkdir -p $out
    cp -r ./. $out/
  '';
}
