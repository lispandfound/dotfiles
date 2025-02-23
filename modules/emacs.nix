{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs30-pgtk
    hunspell
    hunspellDicts.en-au
    sqlite
  ];
  home.file."${config.home.homeDirectory}/.config/doom" = {
    source = ../config/doom;
    target = "${config.home.homeDirectory}/.config/doom";
  };
}
