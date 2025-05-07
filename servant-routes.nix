{ compiler ? "ghc984"
}:
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };
  packages = import ./nix/packages.nix;

  mapAttrs = nixpkgs.lib.mapAttrs;
in
mapAttrs (name: path: builtins.getAttr name nixpkgs.haskellPackages) packages
