{ config, pkgs, lib, ... }:

let
  containerName = "media-server";
  orgGid = 5555;
in {

  systemd.tmpfiles.rules = [ "d /var/lib/dufs 0750 dufs dufs -" ];

  users.groups.org = { gid = orgGid; };

  containers.${containerName} = {
    autoStart = true;
    ephemeral = false;
    users.groups.org = { gid = orgGid; };
    users.users.caddy = {
      isSystemUser = true;
      group = "caddy";
      extraGroups = [ "org" ];
    };
    bindMounts."/var/lib/org" = {
      hostPath = "/var/lib/org";
      isReadOnly = true;
    };

    config = { config, pkgs, ... }: {
      networking.firewall.allowedTCPPorts = [ 80 ];

      services.caddy = {
        enable = true;
        package = pkgs.caddy;

        virtualHosts."media.tail7ee4b1.ts.net" = {
          extraConfig = ''
            root * /var/lib/org
            file_server
            tls internal
          '';
        };
      };
    };
  };
}

