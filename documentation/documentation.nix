{pkgs}:

pkgs.buildNpmPackage {
  name = "@monorepo/hello";

  version = "1.0.0";

  src = ./..;

  npmDepsHash = "sha256-cr9/U4t3ERerkb22IBvCU1x/EUOhPLILS53dESYsJlk=";
  # npmDepsHash = "${pkgs.lib.fakeHash}";

  # src = pkgs.nix-gitignore.gitignoreSourcePure [ "page.nix" ] ./.;

  npmBuild = "npm run build";

  installPhase = ''
    mkdir --parents $out
    ls
    cp --archive ./documentation/build/client/. $out
  '';
}
