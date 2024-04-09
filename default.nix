let

  nixpkgs = import sources.nixpkgs { inherit config; };

  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  sources = import ./nix/sources.nix;

  mkOverrides = super: dontCheck: {
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc928.override {
        overrides = self: super:
          let dontCheck = pkgs.haskell.lib.dontCheck;
          in mkOverrides super dontCheck;
      };
    };
  };

in
  nixpkgs.haskellPackages.developPackage {
    root = ./.;
    name = "servant-routes";
    returnShellEnv = false;
  }
