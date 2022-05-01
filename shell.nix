{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, packages ? import ./. { inherit system enableHaskellProfiling; }
}:

let

  inherit (packages) pkgs marlowe;
  inherit (marlowe) haskell;

  localInputs = (with marlowe; [
    cabal-install
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
  # pointfree
    updateMaterialized
  ]);

in

  haskell.project.shellFor {
    nativeBuildInputs = localInputs;
    withHoogle = false;
  }
