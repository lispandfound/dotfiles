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
      nesi = {
        hostname = "login.hpc.nesi.org.nz";
        user = "jfa92";
        proxyJump = "nesilander";
      };
      vps = {
        hostname = "139.99.195.31";
        user = "jake";
        port = 56303;
      };

      nesilander = {
        hostname = "lander.hpc.nesi.org.nz";
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
