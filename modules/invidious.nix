{ config, pkgs, lib, ... }: {
  # Ensure /var/lib/invidious is created and persists on the host system
  systemd.tmpfiles.rules =
    [ "d /var/lib/invidious 0750 invidious invidious -" ];

  # Ensure the invidious user and group exist on the host
  users.users.invidious = {
    isSystemUser = true;
    group = "invidious";
    uid = 5005;
  };
  users.groups.invidious = { };

  containers.invidiousContainer = {
    autoStart = true;
    ephemeral = true; # Reset on restart to minimize attack persistence

    bindMounts = {
      "/data" = {
        hostPath = "/var/lib/invidious"; # Host data directory
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      environment.systemPackages = with pkgs; [ invidious ];

      networking.firewall.allowedTCPPorts = [ 5005 ];

      # Ensure the invidious user exists inside the container
      users.users.invidious = {
        isSystemUser = true;
        group = "invidious";
        uid = 5005;
      };
      users.groups.invidious = { };

      # Use systemd service script to start the invidious service
      services.invidious = {
        enable = true;
        port = 5005;
        address = "0.0.0.0";
      };
    };
  };
}
