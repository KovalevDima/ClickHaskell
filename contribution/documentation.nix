{pkgs}:

pkgs.stdenv.mkDerivation {
  name = "documentation";
  src = pkgs.lib.fileset.toSource {
    root = ../.;
    fileset = ../documentation;
  };

  dontBuild = true;
  installPhase = ''
    mkdir -p $out
    cp -r ./. $out/
  '';
}
