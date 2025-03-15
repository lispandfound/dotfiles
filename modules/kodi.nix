{ config, pkgs, lib, ... }: {
  # Ensure /var/lib/dufs is created and persists on the host system

  environment.systemPackages = with pkgs; [ kodi-gbm ];

}

