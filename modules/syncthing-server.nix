{ config, lib, pkgs, ... }:

{
  users.users.syncthing = {
    isSystemUser = true;
    home = "/var/lib/syncthing";
    createHome = true;
  };

  services.syncthing = {
    enable = true;
    user = "syncthing";
    dataDir = "/var/lib/syncthing";
    settings = {
      guiAddress =
        "0.0.0.0:8384"; # Change to localhost if remote access isn't needed
    };
  };

  networking.firewall.allowedTCPPorts = [ 22000 8384 ];
  networking.firewall.allowedUDPPorts = [ 21027 ];
}

