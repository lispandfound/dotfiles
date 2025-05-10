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
      vps = {
        hostname = "139.99.195.31";
        user = "jake";
        port = 56303;
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

      "2p" = {
        hostname = "ucquakecore2p";
        user = "qcadmin";
      };
      tacc = {
        hostname = "login2.stampede3.tacc.utexas.edu";
        user = "jfaulkner5782";
      };
      mantle = {
        hostname = "mantle";
        user = "jake";
      };
      lightsail = {
        hostname = "3.105.18.205";
        user = "bitnami";
      };
    };
  };
}
