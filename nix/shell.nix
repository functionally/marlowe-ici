{ repoRoot, inputs, pkgs, system, lib }:

cabalProject:

{
  name = "marlowe-ici";

  packages = with pkgs; [
    pandoc
    ipfs
    remarshal
  ];

  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    nixpkgs-fmt.enable = true;
    shellcheck.enable = true;
    fourmolu.enable = true;
    fourmolu.extraOptions = "-o -XCPP";
    hlint.enable = true;
  };
}
