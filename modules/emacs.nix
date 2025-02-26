{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs30-pgtk
    hunspell
    hunspellDicts.en-au
    sqlite
  ];
  home.file."${config.home.homeDirectory}/.config/emacs" = {
    source = ../config/emacs;
    target = "${config.home.homeDirectory}/.config/emacs";
  };
}
