{ config, pkgs, lib, ... }:

{
  # Enable NAT for container networking (optional if you still need NAT)
  containers.dufsContainer = {
    autoStart = true; # Start the container automatically on boot
    ephemeral = true; # The container will be ephemeral

    # Bind mount /data to /var/run/data inside the container
    bindMounts = {
      "/var/run/data" =
        "/data"; # Host's /data mounted to container's /var/run/data
    };
    # Set the container's configuration
    config = { config, pkgs, lib, ... }: {

      # Services within the container
      system.stateVersion = "24.11"; # Adjust for the desired NixOS version

      # Configure dufs as the container's main process
      environment.systemPackages = with pkgs; [ dufs ];

      # Networking and Firewall settings for container
      networking.firewall.allowedTCPPorts = [ 5000 ]; # Expose port 5000

      # Running the dufs service
      systemd.services.dufs = {
        description = "dufs data sync";
        wantedBy = [ "multi-user.target" ];
        serviceConfig.ExecStart = "${pkgs.dufs}/bin/dufs /data -A";
        serviceConfig.Restart = "always"; # Ensure dufs restarts if it fails
      };
    };
  };
}

