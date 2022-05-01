{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, packages ? import ./. { inherit system enableHaskellProfiling; }
}:
let
  inherit (packages) pkgs marlowe;
  inherit (marlowe) haskell;
  nixFlakesAlias = pkgs.runCommand "nix-flakes-alias" { } ''
    mkdir -p $out/bin
    ln -sv ${pkgs.nixFlakes}/bin/nix $out/bin/nix-flakes
  '';

  # build inputs from nixpkgs ( -> ./nix/default.nix )
  nixpkgsInputs = (with pkgs; [
  ]);

  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with marlowe; [
    cabal-install
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
    updateMaterialized
  ]);

in
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs ++ localInputs;
  withHoogle = false;
}
