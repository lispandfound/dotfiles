{ config, pkgs, lib, ... }: {
  # Define the user mount unit
  systemd.user.services.mount-dufs = {
    Unit = {
      Description = "Mount WebDAV at /mnt/dufs after pinging server";
      After = [
        "network-online.target"
        "tailscaled.service"
        "sys-subsystem-net-devices-tailscale0.device"
      ];
    };
    Install = { WantedBy = [ "default.target" ]; };

    # Ensure the unit will run the mount when logging in
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.mount}/bin/mount /mnt/dufs";
      RemainAfterExit = "true";
    };

  };
}
