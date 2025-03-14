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
        hostPath = "/var/lib/dufs"; # Ensure correct path
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      environment.systemPackages = with pkgs; [ dufs ];

      networking.firewall.allowedTCPPorts =
        [ 5000 ]; # Still exposed—consider firewall rules.

      # Ensure the dufs user exists inside the container
      users.users.dufs = {
        isSystemUser = true;
        group = "dufs";
        uid = 5000;
      };
      users.groups.dufs = { };

      systemd.services.dufs = {
        description = "dufs data sync";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          User = "dufs";
          Group = "dufs";
          ExecStart = "${pkgs.dufs}/bin/dufs /data -A";
          Restart = "always";
        };
      };

    };
  };
}

