{ config, pkgs, lib, ... }: {
  # Ensure /var/lib/flood is created and persists on the host system
  systemd.tmpfiles.rules = [ "d /var/lib/flood 0775 flood flood -" ];

  # Ensure the flood user and group exist on the host
  users.users.flood = {
    isSystemUser = true;
    group = "flood";
    uid = 5006;
  };
  users.groups.flood = { };

  containers.floodContainer = {
    autoStart = true;
    ephemeral = true; # Reset on restart to minimize attack persistence

    bindMounts = {
      "/data" = {
        hostPath = "/var/lib/flood"; # Host data directory (for downloads)
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      environment.systemPackages = with pkgs; [ flood rtorrent ];

      networking.firewall.allowedTCPPorts =
        [ 5006 ]; # Only Flood's web UI needs to be accessible

      # Ensure users exist inside the container
      users.users.flood = {
        isSystemUser = true;
        group = "flood";
        uid = 5006;
      };
      users.groups.flood = { };

      users.users.rtorrent = {
        isSystemUser = true;
        group = "rtorrent";
        uid = 5007;
      };
      users.groups.rtorrent = { };

      # rTorrent config file (downloads go to /data, socket in /tmp)
      environment.etc."rtorrent.rc".text = ''
        directory = /data
        session = /data/rtorrent-session

        # SCGI socket for Flood, now in /tmp
        scgi_local = /tmp/rtorrent.sock
        execute = chmod,770,/tmp/rtorrent.sock

        # Network settings
        port_range = 49160-49161
        port_random = no
        dht = on
        peer_exchange = yes
        use_udp_trackers = yes
      '';

      # systemd service for rTorrent
      systemd.services.rtorrent = {
        description = "rTorrent daemon";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          User = "rtorrent";
          Group = "rtorrent";
          Restart = "always";
          ExecStartPre =
            "${pkgs.coreutils}/bin/rm -f /tmp/rtorrent.sock"; # Ensure no stale socket
          ExecStart =
            "${pkgs.rtorrent}/bin/rtorrent -n -o import=/etc/rtorrent.rc";
        };
      };

      # systemd service for Flood
      systemd.services.flood = {
        description = "Flood Web UI";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "rtorrent.service" ];
        script = ''
          exec ${pkgs.flood}/bin/flood --host 0.0.0.0 --port 5006 --auth none --rtorrent /tmp/rtorrent.sock
        '';
        serviceConfig = {
          User = "flood";
          Group = "flood";
          Restart = "always";
        };
      };
    };
  };
}

