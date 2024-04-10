args@{ compiler ? "ghc928"
}:
let
  inherit (import ./servant-routes.nix args) haskellPackages sources servant-routes;

  pre-commit-check = import ./nix/pre-commit.nix;

  shell = servant-routes.envFunc {};
  headroom-pinned = haskellPackages.callCabal2nix "headroom" sources.headroom {};
in
  shell.overrideAttrs {
      shellHook = ''
        ${pre-commit-check.shellHook}
      '';

      nativeBuildInputs =
        with haskellPackages;
          shell.nativeBuildInputs ++
            [ cabal-install
              haskell-language-server
              hlint
              fourmolu
              ghcid
              headroom-pinned
            ];
    }
