{ compiler ? "ghc928"
}:
let
  inherit (import ./servant-routes.nix {inherit compiler;})
      nixpkgs sources servant-routes;

  pre-commit-check = import ./nix/pre-commit.nix;

  shell = servant-routes.envFunc {};
  headroom-pinned = nixpkgs.haskellPackages.callCabal2nix "headroom" sources.headroom {};
in
  shell.overrideAttrs {
      shellHook = ''
        ${pre-commit-check.shellHook}
      '';

      nativeBuildInputs =
        with nixpkgs.haskellPackages;
          shell.nativeBuildInputs ++
            [ cabal-install
              haskell-language-server
              hlint
              fourmolu
              ghcid
              headroom-pinned
            ];
    }
