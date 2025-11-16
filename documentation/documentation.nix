{pkgs}:

pkgs.buildNpmPackage {
  name = "@monorepo/hello";

  version = "1.0.0";

  src = ./..;

  # npmDepsHash = "sha256-RhuE7+/xMUy4YWi+qayBcsUSkJJlywnxyCDkRvKnk3s=";
  npmDepsHash = "${pkgs.lib.fakeHash}";

  # src = pkgs.nix-gitignore.gitignoreSourcePure [ "page.nix" ] ./.;

  npmBuild = "npm run build";

  installPhase = ''
    mkdir --parents $out
    ls
    cp --archive ./documentation/build/client/. $out
  '';
}
