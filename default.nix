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
, checkMaterialization ? false
, enableHaskellProfiling ? false
, source-repo-override ? { }
}:

let
  inherit (packages) pkgs marlowe sources;
in
  rec {
    inherit pkgs marlowe sources;
  }
