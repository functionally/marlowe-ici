{
  description = "Marlowe ICI";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          marloweICI =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              # This is used by `nix develop .` to open a shell for use with `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal                   = {};
                ghcid                   = {};
                haskell-language-server = {};
                hie-bios                = {};
                hlint                   = {};
                pointfree               = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                openjdk
#               nodejs-17_x
#               go_1_17
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.marloweICI.flake {
      };
    in flake // {
      # Built by `nix build .` or `nix build .#marlowe-ici:exe:marlowe-ici`.
      defaultPackage = flake.packages."marlowe-ici:exe:marlowe-ici";
    });
}
