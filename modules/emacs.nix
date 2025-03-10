{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    enchant
    emacsPackages.jinx
    emacs-pgtk
    hunspell
    hunspellDicts.en-au
    sqlite
  ];
}
