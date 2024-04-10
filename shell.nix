let
  pkg = import ./. {};
  nixpkgs = import sources.nixpkgs {};
  pre-commit-check = import ./nix/pre-commit.nix;
  sources = import ./nix/sources.nix;
in
  (pkg.envFunc {}).overrideAttrs {
      shellHook = ''
        ${pre-commit-check.shellHook}
      '';
    }
