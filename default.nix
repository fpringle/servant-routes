args@{ compiler ? "ghc928" }:
let pkg = import ./servant-routes.nix args;
in  pkg.servant-routes
