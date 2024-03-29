{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "marlowe-ici";
    src = ../.;
    compiler-nix-name = "ghc928";
    shell.withHoogle = false;
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
    };
    modules = [{
      packages = {
        marlowe-ici.ghcOptions = [ "-Werror" ];
        strict-containers.ghcOptions = [ "-Wno-noncanonical-monad-instances" "-Wno-error=noncanonical-monad-instances" ];
      };
    }];
  });

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
