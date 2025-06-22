{ config, lib, pkgs, ... }:

let firmwarePkg = pkgs.linux-firmware;
in {
  options = {
    services.intelAlderLakeWifi.enable =
      lib.mkEnableOption "Intel Alder Lake-S Wi-Fi support";
  };

  config = lib.mkIf config.services.intelAlderLakeWifi.enable {
    hardware.enableAllFirmware = true;

    # Ensure correct firmware package is installed
    hardware.firmware = [ firmwarePkg ];

    # Kernel module options
    boot.extraModprobeConfig = ''
      options iwlwifi power_save=0
      options iwlwifi bt_coex_active=1
      options iwlmvm power_scheme=1
    '';

    # Ensure the driver is loaded early
    boot.kernelModules = [ "iwlwifi" ];

    # NetworkManager (recommended)
    networking.networkmanager.enable = true;

    # Optional: Block wpa_supplicant if using NetworkManager
    networking.wireless.enable = false;
  };
}

