{ config, pkgs, lib, ... }: { # Enabling davfs2 service to allow WebDAV mounting

  # Creating a systemd mount unit for WebDAV
  systemd.mounts = [{
    where = "/mnt/dufs";
    what = "http://media:5000";
    options = "uid=1000,file_mode=0664,dir_mode=2775";
    type = "davfs";
  }];
  systemd.automounts = [{
    description = "Automount for dufs...";
    requires = [ "network-online.target" ];
    after = [ "network-online.target" ];
    where = "/mnt/dufs";
    wantedBy = [ "multi-user.target" ];
    enable = true;
  }];

  users.groups.davfs2 = { };
  environment.systemPackages = [ pkgs.davfs2 ];
}
