{ config, pkgs, ... }:

let
  # Define a dedicated system user for running the Docker container
  serviceUser = "mapbin";

  # Define the name of your Docker image
  mapbinImage = "lispandfound/mapbin:latest";

  # Define the path for persistent map data
  # Using a dedicated directory that the system user can access
  mapDataPath = "/var/lib/mapbin/map";
in {
  # 1. Enable Docker (standard mode, not rootless)
  virtualisation.docker = { enable = true; };

  # 2. Configure a dedicated system user for the mapbin service
  users.users.${serviceUser} = {
    isSystemUser = true;
    group = serviceUser;
    description = "MapBin service user";
    home = "/var/lib/mapbin";
    createHome = true;
  };

  # Create a matching group
  users.groups.${serviceUser} = { };

  # 3. Define a systemd service for the mapbin application
  # This will run as the dedicated system user
  systemd.services.mapbin = {
    description = "MapBin Application Service";
    after = [ "docker.service" "network-online.target" "wg-quick-wg0.service" ];
    requires = [ "docker.service" "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    # Create the persistent data directory before starting
    preStart = ''
      ${pkgs.coreutils}/bin/mkdir -p ${mapDataPath}
      ${pkgs.coreutils}/bin/chown -R ${serviceUser}:${serviceUser} ${mapDataPath}
      ${pkgs.docker}/bin/docker pull ${mapbinImage}
    '';

    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "5s";

      # Run as the dedicated system user
      User = "${serviceUser}";
      Group = "${serviceUser}";

      # Add the system user to the docker group so it can run docker commands
      SupplementaryGroups = [ "docker" ];

      # Run Docker container
      ExecStart = ''
        ${pkgs.docker}/bin/docker run \
          --rm \
          --name mapbin-container \
          -p 127.0.0.1:3000:3000 \
          -v ${mapDataPath}:/mapbin/map \
          ${mapbinImage}
      '';

      # Command to stop the container
      ExecStop = "${pkgs.docker}/bin/docker stop mapbin-container";
    };
  };

  # 4. Firewall configuration for NixOS
  # Ensure your NixOS firewall allows incoming traffic to port 3000 on the WireGuard interface
  networking.firewall.enable = true;
  networking.firewall.interfaces."wg0".allowedTCPPorts = [ 3000 ];

  # Ensure the system user is part of the docker group
  users.users.${serviceUser}.extraGroups = [ "docker" ];
}
