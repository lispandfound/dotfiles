{ config, lib, pkgs, ... }:

{
  boot = {
    kernel.sysctl = {
      "kernel.unprivileged_userns_clone" = 1; # for apptainer
    };
  };
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };
  programs.singularity = {
    enable = true;
    package = pkgs.apptainer;
    enableSuid = true;
    enableFakeroot = true;
  };
}
