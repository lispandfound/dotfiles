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
      "/var/lib/flood" = {
        hostPath = "/var/lib/flood"; # Host data directory (for downloads)
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      environment.systemPackages = with pkgs; [ flood rtorrent findutils ];

      networking.firewall.allowedTCPPorts =
        [ 5006 ]; # Only Flood's web UI needs to be accessible

      # Ensure flood user exists inside the container
      users.users.flood = {
        isNormalUser = true;
        group = "flood";
        uid = 5006;
      };

      systemd.services.flood-clean = {
        description = "Clean up old downloads in Flood";
        serviceConfig = {
          Type = "oneshot";
          User = "flood";
          Group = "flood";
          ExecStart =
            "${pkgs.findutils}/bin/find /var/lib/flood/download -type f -mtime +7 -delete";
        };
      };

      # Systemd timer to trigger cleanup daily
      systemd.timers.flood-clean = {
        description = "Run flood-clean service daily";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "03:00:00"; # Run every day at 3 AM
          Persistent = true;
        };
      };

      users.groups.flood = { };
      # rTorrent config file (downloads go to /data, socket in /tmp)
      environment.etc."rtorrent.rc".text = ''
        #############################################################################
        # A minimal rTorrent configuration that provides the basic features
        # you want to have in addition to the built-in defaults.
        #
        # See https://github.com/rakshasa/rtorrent/wiki/CONFIG-Template
        # for an up-to-date version.
        #############################################################################
        system.daemon.set = true

        ## Instance layout (base paths)
        method.insert = cfg.basedir,  private|const|string, (cat,"/var/lib/flood/")
        method.insert = cfg.download, private|const|string, (cat,(cfg.basedir),"download/")
        method.insert = cfg.logs,     private|const|string, (cat,(cfg.basedir),"log/")
        method.insert = cfg.logfile,  private|const|string, (cat,(cfg.logs),"rtorrent-",(system.time),".log")
        method.insert = cfg.session,  private|const|string, (cat,(cfg.basedir),".session/")
        method.insert = cfg.watch,    private|const|string, (cat,(cfg.basedir),"watch/")


        ## Listening port for incoming peer traffic (fixed; you can also randomize it)
        network.port_range.set = 50000-50000
        network.port_random.set = no


        ## Tracker-less torrent and UDP tracker support
        ## (conservative settings for 'private' trackers, change for 'public')
        dht.mode.set = auto
        protocol.pex.set = 1

        trackers.use_udp.set = 1


        ## Peer settings
        throttle.max_uploads.set = 100
        throttle.max_uploads.global.set = 250

        throttle.min_peers.normal.set = 20
        throttle.max_peers.normal.set = 60
        throttle.min_peers.seed.set = 30
        throttle.max_peers.seed.set = 80
        trackers.numwant.set = 80

        protocol.encryption.set = allow_incoming,try_outgoing,enable_retry


        ## Limits for file handle resources, this is optimized for
        ## an `ulimit` of 1024 (a common default). You MUST leave
        ## a ceiling of handles reserved for rTorrent's internal needs!
        network.http.max_open.set = 50
        network.max_open_files.set = 600
        network.max_open_sockets.set = 300


        ## Memory resource usage (increase if you have a large number of items loaded,
        ## and/or the available resources to spend)
        pieces.memory.max.set = 1800M
        network.xmlrpc.size_limit.set = 4M


        ## Basic operational settings (no need to change these)
        session.path.set = (cat, (cfg.session))
        directory.default.set = (cat, (cfg.download))
        log.execute = (cat, (cfg.logs), "execute.log")
        #log.xmlrpc = (cat, (cfg.logs), "xmlrpc.log")
        execute.nothrow = sh, -c, (cat, "echo >",\
            (session.path), "rtorrent.pid", " ",(system.pid))


        ## Other operational settings (check & adapt)
        encoding.add = UTF-8
        system.umask.set = 0027
        system.cwd.set = (directory.default)
        network.http.dns_cache_timeout.set = 25
        schedule2 = monitor_diskspace, 15, 60, ((close_low_diskspace, 1000M))


        ## Some additional values and commands
        method.insert = system.startup_time, value|const, (system.time)
        method.insert = d.data_path, simple,\
            "if=(d.is_multi_file),\
                (cat, (d.directory), /),\
                (cat, (d.directory), /, (d.name))"
        method.insert = d.session_file, simple, "cat=(session.path), (d.hash), .torrent"


        ## Watch directories (add more as you like, but use unique schedule names)
        ## Add torrent
        schedule2 = watch_load, 11, 10, ((load.verbose, (cat, (cfg.watch), "load/*.torrent")))
        ## Add & download straight away
        schedule2 = watch_start, 10, 10, ((load.start_verbose, (cat, (cfg.watch), "start/*.torrent")))


        ## Run the rTorrent process as a daemon in the background
        ## (and control via XMLRPC sockets)
        #system.daemon.set = true
        network.scgi.open_local = "/tmp/rtorrent.sock"
        #execute.nothrow = chmod,770,(cat,(session.path),rpc.socket)


        ## Logging:
        ##   Levels = critical error warn notice info debug
        ##   Groups = connection_* dht_* peer_* rpc_* storage_* thread_* tracker_* torrent_*
        print = (cat, "Logging to ", (cfg.logfile))
        log.open_file = "log", (cfg.logfile)
        log.add_output = "info", "log"
        #log.add_output = "tracker_debug", "log"

        ### END of rtorrent.rc ###
      '';

      # systemd service for rTorrent
      systemd.services.rtorrent = {
        description = "rTorrent daemon";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          User = "flood";
          Group = "flood";
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
          exec ${pkgs.flood}/bin/flood --host 0.0.0.0 --port 5006 --auth none --rtsocket /tmp/rtorrent.sock
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

