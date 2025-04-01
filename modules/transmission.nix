{ config, pkgs, lib, ... }: {
  # Ensure /var/lib/transmission is created and persists on the host system
  systemd.tmpfiles.rules =
    [ "d /var/lib/transmission 0775 transmission transmission -" ];

  # Ensure the transmission user and group exist on the host
  users.users.transmission = {
    isSystemUser = true;
    group = "transmission";
    uid = 70;
  };
  users.groups.transmission = { };

  containers.transmissionContainer = {
    autoStart = true;
    ephemeral = true; # Reset on restart to minimize attack persistence

    bindMounts = {
      "/var/lib/transmission" = {
        hostPath =
          "/var/lib/transmission"; # Host data directory (for downloads)
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      environment.systemPackages = with pkgs; [ transmission_4 findutils ];

      # Ensure transmission user exists inside the container

      systemd.services.transmission-clean = {
        description = "Clean up old downloads in transmission";
        serviceConfig = {
          Type = "oneshot";
          User = "transmission";
          Group = "transmission";
          ExecStart =
            "${pkgs.findutils}/bin/find /var/lib/transmission/download -type f -mtime +7 -delete";
        };
      };

      # Systemd timer to trigger cleanup daily
      systemd.timers.transmission-clean = {
        description = "Run transmission-clean service daily";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "03:00:00"; # Run every day at 3 AM
          Persistent = true;
        };
      };

      users.groups.transmission = { };

      # systemd service for Transmission
      services.transmission = {
        enable = true;
        package = pkgs.transmission_4;
        openRPCPort = true; # Open firewall for RPC
        settings = {
          rpc-bind-address = "0.0.0.0"; # Bind to own IP
          rpc-whitelist =
            "127.0.0.1"; # Whitelist your remote machine (10.0.0.1 in this example)
          rpc-whitelist-enabled = false;
          rpc-host-whitelist-enabled = false;
        };
        user = "transmission";
        group = "transmission";

      };
      # see https://github.com/NixOS/nixpkgs/issues/258793#issuecomment-2647700359      
      systemd.services.transmission.serviceConfig = {
        RootDirectoryStartOnly = lib.mkForce false;
        RootDirectory = lib.mkForce "";
        PrivateMounts = lib.mkForce false;
        PrivateUsers = lib.mkForce false;
      };
      # systemd service for transmission
      networking.firewall = rec {
        allowedTCPPortRanges = [
          {
            from = 5006;
            to = 5006;
          }
          {
            from = 9091;
            to = 9091;
          }
          {
            from = 6881;
            to = 6999;
          }
        ];
        allowedUDPPortRanges = allowedTCPPortRanges;
      };
    };
  };

  networking.firewall = rec {
    allowedTCPPortRanges = [{
      from = 6881;
      to = 6999;
    }];
    allowedUDPPortRanges = allowedTCPPortRanges;
  };
}

