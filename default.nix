########################################################################
# default.nix -- The top-level nix build file for Marlowe.
#
# This file defines various attributes that are used for building and
# developing Marlowe.
#
########################################################################
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, haskellNix ? import sources.haskell-nix {
    pkgs = import sources.nixpkgs { inherit system; };
    sourcesOverride = {
      hackage = sources.hackage-nix;
      stackage = sources.stackage-nix;
    };
  }
, packages ? import ./nix { inherit system sources crossSystem config sourcesOverride haskellNix checkMaterialization enableHaskellProfiling source-repo-override; }
  # An explicit git rev to use, passed when we are in Hydra
  # Whether to check that the pinned shas for haskell.nix are correct. We want this to be
  # false, generally, since it does more work, but we set it to true in the CI
, checkMaterialization ? false
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
, source-repo-override ? { }
}:
let
  inherit (packages) pkgs marlowe sources;
  inherit (marlowe) haskell;
  inherit (haskell.packages.cardano-wallet.components.exes) cardano-wallet;
  inherit (haskell.packages.plutus-chain-index.components.exes) plutus-chain-index;
  inherit (haskell.packages.marlowe-dashboard-server.components.exes) marlowe-dashboard-server;
in
rec {
  inherit pkgs marlowe;
}
