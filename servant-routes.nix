{ compiler ? "ghc984"
}:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit config; };

  haskellPackages = nixpkgs.haskellPackages;

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
    nixpkgs.haskellPackages.developPackage {
      root = ./.;
      name = "servant-routes";
      returnShellEnv = false;
    };

  inherit
    sources
    nixpkgs
    compiler;
}
