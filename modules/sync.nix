{ config, pkgs, lib, ... }: { # Enabling davfs2 service to allow WebDAV mounting
  services.davfs2.enable = true;

  # Creating a systemd mount unit for WebDAV
  systemd.mounts = [{
    # The unit file name (you can give it a name for your mount)
    mountPoint = "/mnt/dufs";
    # The URL to mount from
    device = "http://media:5000";
    # The options for davfs
    options = "uid=1000,file_mode=0664,dir_mode=2775,grpid";
    # Type of mount (WebDAV)
    type = "davfs";
    # Timeout for mounting
    timeout = "15";
    # Dependencies (ensure network is up before mounting)
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
  }];
}
