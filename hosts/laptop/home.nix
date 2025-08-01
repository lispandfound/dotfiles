{ inputs, config, pkgs, ... }:

{
  imports = [
    ../../modules/helix.nix
    ../../modules/emacs.nix
    ../../modules/shell.nix
    ../../modules/git.nix
    ../../modules/kitty.nix
    ../../modules/python.nix
    ../../modules/ssh.nix
  ];
  home.username = "jake";
  home.homeDirectory = "/home/jake";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    keepassxc
    syncthing
    kdotool
    safeeyes
    slack
    zoom-us
  ];

  # programs.gnome-shell = {
  #   enable = true;
  #   extensions = [
  #     { package = pkgs.gnomeExtensions.gsconnect; }
  #     { package = pkgs.gnomeExtensions.hide-minimized; }
  #     { package = pkgs.gnomeExtensions.appindicator; }
  #   ];
  # };

  services.kdeconnect.enable = true;

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
