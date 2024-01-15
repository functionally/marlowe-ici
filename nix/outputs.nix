{ repoRoot, inputs, pkgs, system, lib }:
let
  project = repoRoot.nix.project;
in
[
  # Default packages, apps, devShells, checks, hydraJobs coming from the Haskell project
  (
    project.flake
  )
  { }
]
