{ compiler ? "ghc984"
}:
(import ./servant-routes.nix { inherit compiler; })
