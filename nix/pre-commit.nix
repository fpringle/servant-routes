let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs {};
  nix-pre-commit-hooks =
    import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");
  haskellFilePattern = "\\.l?hs(-boot)?$";
  haskellSrcFilePattern = "^.*/src.*\\.l?hs(-boot)?$";
  hlint = "${nixpkgs.haskellPackages.hlint}/bin/hlint";
  fourmolu = "${nixpkgs.haskellPackages.fourmolu}/bin/fourmolu";

  headroom-pinned = nixpkgs.haskellPackages.callCabal2nix "headroom" sources.headroom {};
  headroom = "${headroom-pinned}/bin/headroom";
  xargs = "${nixpkgs.findutils}/bin/xargs";
  headroom-script =
    nixpkgs.writeShellScriptBin "run-headroom"
      ''
      set -euo pipefail
      files="$@"
      echo $files | ${xargs} -n1 echo | ${xargs} -I{} echo '-s {}' | ${xargs} ${headroom} run
      '';
in
  nix-pre-commit-hooks.run {
    src = ./.;

    hooks = {
      "0-hlint" = {
        name = "hlint";
        enable = true;
        description =
          "HLint gives suggestions on how to improve your source code.";
        entry = "bash -c 'for n in $(seq 0 \"$#\"); do ${hlint} --refactor --refactor-options=\"-i\" \"\${!n}\"; done'";
        files = haskellFilePattern;
      };

      "1-fourmolu" = {
        name = "fourmolu";
        enable = true;
        description = "Haskell code prettifier.";
        entry = "${fourmolu} --mode inplace";
        files = haskellFilePattern;
      };

      "2-headroom" = {
        name = "headroom";
        enable = true;
        description = "Manage your source code license headers.";
        entry = "${headroom-script}/bin/run-headroom";
        files = haskellSrcFilePattern;
      };

      # by default, pre-commit fails if a hook modifies files, but doesn't
      # tell us which files have been modified. Smart, right?
      # this workaround runs a `git diff` to print any files that have
      # been modified by previous hooks.
      # NOTE: this should always be the last hook run, so when adding hooks
      # make sure to add them above this one.
      "3-git-diff" = {
        name = "git diff";
        enable = true;
        entry = "git diff --name-only --exit-code";
        language = "system";
        pass_filenames = true;
      };
    };
  }
