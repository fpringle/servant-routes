let
  inherit (import ./utils.nix) findHaskellPackages;
in
findHaskellPackages ../.
