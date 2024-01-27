{ repoRoot, inputs, pkgs, lib, system }:
{
  iciContainer = lib.iogx.mkContainerFromCabalExe {
    exe = inputs.self.packages.marlowe-ici;
    packages = [
      pkgs.bash
      pkgs.kubo
    ];
  };
}
