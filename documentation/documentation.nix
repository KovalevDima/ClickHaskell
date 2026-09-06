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
    hash = "sha256-Q3tJrPZXFLSrQL4SDm/zv8u+y6K4Bhgkk/AqZB/u9jE=";
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
