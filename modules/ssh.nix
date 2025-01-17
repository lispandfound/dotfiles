{ config, pkgs, ... }:

{
  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPersist = "yes";
    serverAliveInterval = 300;
    serverAliveCountMax = 2;
    controlPath = "~/.ssh/sockets/ssh_mux_%h_%p_%r";
    matchBlocks = {
      mahuika = {
        hostname = "login.mahuika.nesi.org.nz";
        user = "jfa92";
        proxyJump = "lander2";
      };

      maui = {
        hostname = "login.maui.nesi.org.nz";
        user = "jfa92";
        proxyJump = "lander2";
      };

      lander2 = {
        hostname = "lander02.nesi.org.nz";
        user = "jfa92";
      };

      hyp = {
        hostname = "hypocentre";
        user = "jfa92";
      };
    };
  };
}
