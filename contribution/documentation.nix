{pkgs, compiler}:

pkgs.stdenv.mkDerivation {
  name = "documentation";
  src = pkgs.nix-gitignore.gitignoreSourcePure [] ./..;

  buildPhase = "${compiler} build --verbose";

  installPhase = ''
    mkdir -p "$out"
    cp -r ./_site/. "$out"
  '';
}
