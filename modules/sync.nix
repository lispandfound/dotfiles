{ config, pkgs, lib, ... }: {
  environment.systemPackages = with pkgs; [ davfs2 mount umount ];

  users.users.davfs2 = {
    isSystemUser = true;
    group = "davfs2";
  };
  users.groups.davfs2 = { };

  # Mount unit for WebDAV (delayed until needed)
  systemd.services.mount-dufs = {
    description = "WebDAV Mount for dufs";
    wants = [ "tailscaled.service" ]; # Ensures Tailscale is up
    after = [ "tailscaled.service" ]; # Ensures it starts after Tailscale
    wantedBy = [ "graphical.target" ]; # Ensures it's available after boot

    serviceConfig = {
      Type = "oneshot"; # Runs once at startup
      RemainAfterExit = true; # Keeps the mount persistent
      ExecStart =
        "${pkgs.mount}/bin/mount -t davfs -o uid=1000,file_mode=0664,dir_mode=2775 https://media.tail7ee4b1.ts.net:5000 /mnt/dufs";
      ExecStop = "${pkgs.umount}/bin/umount /mnt/dufs";
    };
  };
}
