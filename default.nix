{ compiler ? "ghc928"
}:
let

  nixpkgs = import sources.nixpkgs { inherit config; };

  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  sources = import ./nix/sources.nix;
  headroom-pinned = haskellPackages.callCabal2nix "headroom" sources.headroom {};

  haskellPackages = nixpkgs.haskell.packages.${compiler};

  mkOverrides = super: dontCheck: {
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.${compiler}.override {
        overrides = self: super:
          let dontCheck = pkgs.haskell.lib.dontCheck;
          in mkOverrides super dontCheck;
      };
    };
  };

in
  haskellPackages.developPackage {
    root = ./.;
    name = "servant-routes";
    returnShellEnv = false;
    modifier = drv:
      nixpkgs.haskell.lib.addBuildTools drv (with haskellPackages;
        [ cabal-install
          haskell-language-server
          hlint
          fourmolu
          ghcid
          headroom-pinned
        ]);
  }
