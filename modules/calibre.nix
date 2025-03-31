{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ calibre ];

  # Optional: Enable Calibre's content server if desired.
  # services.calibre-server = {
  #   enable = true;
  #   port = 8080; # Or another port
  #   # Add any other Calibre server options here.
  # };

  # Optional: Configure Calibre's library location (if you don't want the default).
  # environment.variables.CALIBRE_CONFIG_DIRECTORY = "/path/to/calibre/config";
  # environment.variables.CALIBRE_LIBRARY_PATH = "/path/to/calibre/library";
  hardware.udev.extraRules = ''
    # Example for a Kindle device:
    SUBSYSTEM=="usb", ATTRS{idVendor}=="1949", MODE="0666"

    # Udev rule for Kobo devices:
    SUBSYSTEM=="usb", ATTRS{idVendor}=="2237", MODE="0666"
  '';
  # Optional: Add udev rules for ebook readers.
  # hardware.udev.extraRules = ''
  #   # Example for a Kindle device:
  #   SUBSYSTEM=="usb", ATTRS{idVendor}=="1949", MODE="0666"
  # '';

  # Optional: Install plugins.
  # environment.systemPackages = with pkgs; [
  #   calibre.withPlugins (plugins: [
  #     plugins.DeDRM_tools
  #     # Add other plugins here
  #   ])
  # ];

  # Optional: Enable systemd service for content server.
  # systemd.user.services.calibre-server = {
  #   enable = config.services.calibre-server.enable;
  #   description = "Calibre Content Server";
  #   after = [ "network.target" ];
  #   wantedBy = [ "multi-user.target" ];
  #   serviceConfig = {
  #     ExecStart = "${pkgs.calibre}/bin/calibre-server --port ${toString config.services.calibre-server.port}";
  #     Restart = "always";
  #     User = "yourusername"; #replace with your username.
  #     Group = "users"; #replace with group if needed.
  #   };
  # };
}
