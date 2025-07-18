{pkgs}:

pkgs.buildNpmPackage {
  name = "@monorepo/hello";

  version = "1.0.0";

  src = ./..;

  npmDepsHash = "sha256-/daW6oHsF2SvnbfedP5PahvNRYzOmvbIKDEgePjsxqo=";
  # npmDepsHash = "${pkgs.lib.fakeHash}";

  # src = pkgs.nix-gitignore.gitignoreSourcePure [ "page.nix" ] ./.;

  npmBuild = "npm run build";

  installPhase = ''
    mkdir --parents $out
    ls
    cp --archive ./documentation/build/client/. $out
  '';
}
