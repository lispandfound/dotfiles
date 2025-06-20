{ inputs, config, pkgs, ... }:

{
  imports = [
    ../../modules/emacs.nix
    ../../modules/helix.nix
    ../../modules/shell.nix
    ../../modules/git.nix
    ../../modules/kitty.nix
  ];

  home.username = "jake";
  home.homeDirectory = "/home/jake";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs;
    [

    ];

  programs.gnome-shell = {
    enable = true;
    extensions = [
      { package = pkgs.gnomeExtensions.gsconnect; }
      { package = pkgs.gnomeExtensions.hide-minimized; }
      { package = pkgs.gnomeExtensions.dash-to-dock; }
      {
        package =
          pkgs.gnomeExtensions.resolution-and-refresh-rate-in-quick-settings;
      }
    ];
  };

  dconf.settings = {
    "org/gnome/desktop/interface".scaling-factor = 2.0;
    "org/gnome/desktop/a11y/applications".screen-keyboard-enabled = true;
  };
  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;

  services.syncthing = { enable = true; };
}
