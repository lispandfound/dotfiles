{ config, pkgs, lib, ... }: {
  environment.systemPackages = with pkgs; [ davfs2 mount umount ];

  users.users.davfs2 = {
    isSystemUser = true;
    group = "davfs2";
  };
  users.groups.davfs2 = { };
  environment.etc."davfs2/davfs2.conf".text = ''
    use_locks 0
    dav_group davfs2
  '';

  systemd.mounts = [{
    where = "/mnt/dufs";
    what = "https://media.tail7ee4b1.ts.net:5000";
    options = "uid=1000,file_mode=0664,dir_mode=2775";
    type = "davfs";
  }];

  # Mount unit for WebDAV (delayed until needed)
  systemd.automounts = [{
    description = "WebDAV Mount for dufs...";
    requires = [ "tailscaled.service" ];
    after = [
      "tailscaled.service"
      "sys-subsystem-net-devices-tailscale0.device"
    ]; # Ensures Tailscale is up
    wantedBy = [ "multi-user.target" ]; # Ensures it's available after boot
    where = "/mnt/dufs";
    enable = true;
  }];
}
