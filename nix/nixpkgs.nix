{ compiler ? "ghc984"
}:
let
  # set the GHC version for the entire project.

  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { overlays = [ overlay ]; };
  packages = import ./packages.nix;

  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];

  overlay = final: prev:
    let
      inherit (prev.haskell.lib)
        doJailbreak
        doCheck
        appendConfigureFlag
        addBuildDepend
        overrideCabal
        ;

      haskell-overrides = hfinal: hprev:
        let
          # When we pin specific versions of Haskell packages, they'll go here using callCabal2Nix.
          packageOverrides = {
            servant = hfinal.callCabal2nix "servant" "${sources.servant}/servant" {};
          };

          pkgMods = [ ];

          makePackage = name: path:
            let pkg = hfinal.callCabal2nix name (gitignore path) { };
            in nixpkgs.lib.pipe pkg pkgMods;
        in
        builtins.mapAttrs makePackage packages // packageOverrides;
    in
    {
      # This will become the main package set for the project. In a `nix-shell`,
      # this is what we'll have access to.
      haskellPackages = prev.haskell.packages.${compiler}.override {
        overrides = haskell-overrides;
      };
    };
in
nixpkgs
