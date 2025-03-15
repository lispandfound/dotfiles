{ config, pkgs, lib, ... }: {
  # Ensure /var/lib/immich is created and persists on the host system
  systemd.tmpfiles.rules = [ "d /var/lib/immich 0750 immich immich -" ];

  # Ensure the immich user and group exist on the host
  users.users.immich = {
    isSystemUser = true;
    group = "immich";
    uid = 5005;
  };
  users.groups.immich = { };

  containers.immichContainer = {
    autoStart = true;
    ephemeral = true; # Reset on restart to minimize attack persistence

    bindMounts = {
      "/var/lib/immich" = {
        hostPath = "/var/lib/immich"; # Host data directory
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      environment.systemPackages = with pkgs; [ immich ];

      # Ensure the immich user exists inside the container
      users.users.immich = {
        isSystemUser = true;
        group = "immich";
        uid = 5005;
      };
      users.groups.immich = { };

      # Use systemd service script to read the password file and run immich
      services.immich = {
        enable = true;
        port = 5005;
        openFirewall = true;
        host = "0.0.0.0";
      };
    };
  };
}

