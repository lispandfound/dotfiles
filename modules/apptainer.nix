{ config, pkgs, ... }: {
  boot = {
    kernel.sysctl = {
      "kernel.unprivileged_userns_clone" = 1; # for apptainer
    };
  };
  programs.singularity = {
    enable = true;
    package = pkgs.apptainer;
    enableSuid = true;
    enableFakeroot = true;
  };
}
