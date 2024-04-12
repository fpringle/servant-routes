args@{ compiler ? "ghc928" }:
let pkg = import ./servant-routes.nix args;
in {
  inherit (pkg) servant-routes;
  inherit (pkg.servant-routes) doc;
}
