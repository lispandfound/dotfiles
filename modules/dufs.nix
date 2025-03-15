{ config, pkgs, lib, ... }: {
  # Ensure /var/lib/dufs is created and persists on the host system
  systemd.tmpfiles.rules = [ "d /var/lib/dufs 0750 dufs dufs -" ];

  # Ensure the dufs user and group exist on the host
  users.users.dufs = {
    isSystemUser = true;
    group = "dufs";
    uid = 5000;
  };
  users.groups.dufs = { };

  containers.dufsContainer = {
    autoStart = true;
    ephemeral = true; # Reset on restart to minimize attack persistence

    bindMounts = {
      "/data" = {
        hostPath = "/var/lib/dufs"; # Host data directory
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      environment.systemPackages = with pkgs; [ dufs ];

      networking.firewall.allowedTCPPorts = [ 5000 ];

      # Ensure the dufs user exists inside the container
      users.users.dufs = {
        isSystemUser = true;
        group = "dufs";
        uid = 5000;
      };
      users.groups.dufs = { };

      # Use systemd service script to read the password file and run dufs
      systemd.services.dufs = {
        description = "dufs data sync";
        wantedBy = [ "multi-user.target" ];
        script = ''
          # Read the password file and format it as the auth argument for dufs
          DUFS_AUTH_RULES=$(cat /data/dufs-auth.txt | xargs)
          exec ${pkgs.dufs}/bin/dufs /data -A -a "$DUFS_AUTH_RULES"
        '';
        serviceConfig = {
          User = "dufs";
          Group = "dufs";
          Restart = "always";
        };
      };
    };
  };
}

