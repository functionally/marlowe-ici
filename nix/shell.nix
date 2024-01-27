{ repoRoot, inputs, pkgs, system, lib }:

cabalProject:

{
  name = "marlowe-ici";

  packages = with pkgs; [
    pandoc
    ipfs
    remarshal
    inputs.n2c.packages.skopeo-nix2container
  ];

  scripts = {
    publish-container = {
      description = "Publishes the docker image.";
      exec = ''
        VERSION=$(nix run .#marlowe-ici -- --version 2> /dev/null | sed -e 's/^.* //')
        nix build .#containers.x86_64-linux.iciContainer -o image.json
        skopeo copy nix:image.json docker://ghcr.io/functionally/marlowe-ici:$VERSION
      '';
      # enable = isLinux;
      group = "marlowe";
    };
  };

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
