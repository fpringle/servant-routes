let
  nixpkgs = import sources.nixpkgs {};
  nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");
  pre-commit-check = import ./nix/pre-commit.nix;
  sources = import ./nix/sources.nix;
  headroom-pinned = nixpkgs.haskellPackages.callCabal2nix "headroom" sources.headroom {};
in
  with nixpkgs.haskellPackages;
    shellFor {
      packages = p: [(import ./.)];
      buildInputs = [
        cabal-install
        haskell-language-server
        hlint
        fourmolu
        ghcid
        headroom-pinned
      ];
      shellHook = ''
        ${pre-commit-check.shellHook}
      '';
    }
