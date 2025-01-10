{ config, pkgs, ... }:

{

  imports =
    [ ../../modules/helix.nix ../../modules/shell.nix ../../modules/git.nix ];
  home.username = "jfa92";
  home.homeDirectory = "/home/jfa92";

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

}
