{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    enchant
    emacsPackages.jinx
    emacs-pgtk
    (hunspellWithDicts ([ hunspellDicts.en_AU-large ]))
    sqlite
    stow # to symlink emacs directory without having it read-only
  ];
  dconf.settings = {
    "org/gnome/desktop/input-sources".xkb-options = [ "caps:ctrl_modifier" ];
  };
}
