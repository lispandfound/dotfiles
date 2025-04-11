{ config, pkgs, lib, ... }:

let
  containerName = "media-server";
  orgGid = 5555;
  caddyUid = 5556;
in {
  environment.systemPackages = with pkgs; [ python3 ];
  users.groups.org = {
    gid = 5555; # Or let NixOS assign it
  };

  users.users.org = {
    isSystemUser = true;
    group = "org";
    home = "/var/lib/org";
    createHome = true;
  };
  systemd.services.org-static-server = {
    description = "Static file server for /var/lib/org using Python";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart =
        "${pkgs.python3}/bin/python3 -m http.server 5007 --directory /var/lib/org";
      WorkingDirectory = "/var/lib/org";
      User = "org";
      Group = "org";
      Restart = "on-failure";
    };
  };
  networking.firewall.allowedTCPPorts = [ 5007 ];
}

