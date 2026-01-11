{pkgs}:

pkgs.stdenv.mkDerivation (finalAttrs: {

  pname = "@clickhaskell/docs";

  version = "1.0.0";

  src = ./..;
  # src = pkgs.nix-gitignore.gitignoreSourcePure [ "page.nix" ] ./.;

  nativeBuildInputs = with pkgs; [
    nodejs
    pnpm.configHook
  ];

  pnpmDeps = pkgs.pnpm.fetchDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 2;
    hash = "sha256-v7jZpUb5N2xK7+aJW2tQomOGeqPq2QF4rhaREIuNnzY=";
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
