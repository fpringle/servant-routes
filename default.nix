args@{ compiler ? "ghc984" }:
let pkg = import ./servant-routes.nix args;
in  pkg.servant-routes
