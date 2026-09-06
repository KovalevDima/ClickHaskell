{pkgs}:

pkgs.stdenv.mkDerivation (finalAttrs: {

  pname = "@clickhaskell/docs";

  version = "1.0.0";

  src = ./..;
  # src = pkgs.nix-gitignore.gitignoreSourcePure [ "page.nix" ] ./.;

  nativeBuildInputs = with pkgs; [
    nodejs
    pnpm
    pnpmConfigHook
  ];

  pnpmDeps = pkgs.fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 4;
<<<<<<< Updated upstream
    hash = "sha256-uv2/r1kBEYzSSsW6hZfm9xjgqvgiuhRklOxL77i2PPc=";
=======
    hash = "sha256-eVfcmXtV8uKgE1EgqUnfcsiVZrCY1C5764edTRBF28s=";
>>>>>>> Stashed changes
    # hash = "${pkgs.lib.fakeHash}";
  };

  buildPhase = ''
    runHook preBuild

    pnpm --filter=@clickhaskell/docs build

    runHook postBuild
  '';

  installPhase = ''
    mkdir --parents $out
    ls documentation
    cp --archive ./documentation/build/client/. $out
  '';
})
