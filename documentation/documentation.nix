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
    fetcherVersion = 2;
<<<<<<< HEAD
    hash = "sha256-TUMelvji7EXYKDMRdhOXTRMdz3mZg9xmsxFmGt2MAMM=";
=======
    hash = "sha256-f14UGCtP2WrXR5dbQ6g1kWBJmTJK+QJ/8ikEsEvdeSw=";
>>>>>>> 53f6dd8 (Update hash)
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
