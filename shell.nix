let

  project = import ./default.nix;

in

  project.shellFor {

    packages = ps: with ps; [
      marlowe-ici
    ];

    withHoogle = false;

    tools = {
      cabal                   = "latest";
      ghcid                   = "latest";
      haskell-language-server = "latest";
      hie-bios                = "latest";
      hindent                 = "latest";
      hlint                   = "latest";
      pointfree               = "latest";
    };

    buildInputs = [ (import <nixpkgs> {}).git ];

    exactDeps = true;

  }
