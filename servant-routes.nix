{ compiler ? "ghc928"
}:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit config; };

  haskellPackages = nixpkgs.haskell.packages.${compiler};

  mkOverrides = super: {
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.${compiler}.override {
        overrides = self: super: mkOverrides super;
      };
    };
  };
in {
  servant-routes =
    haskellPackages.developPackage {
      root = ./.;
      name = "servant-routes";
      returnShellEnv = false;
    };

  inherit
    sources
    nixpkgs
    compiler
    haskellPackages;
}
