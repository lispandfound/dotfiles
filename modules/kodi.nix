{ config, pkgs, lib, ... }: {

  services.xserver = {
    enable = true;
    desktopManager.kodi = {
      enable = true; # package = pkgs.kodi;
      package = pkgs.kodi-gbm;
    };

  };

  services.xserver.displayManager.lightdm.greeter.enable = false;
  environment.systemPackages = with pkgs; [ kodi-gbm ];

}

