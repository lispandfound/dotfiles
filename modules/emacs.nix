{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    (enchant.override {
      withAspell = false;
      withHspell = false;
    })
    nuspell
    emacsPackages.jinx
    emacs-pgtk
    hunspellDicts.en_AU-large
    sqlite
    stow
  ];

  dconf.settings = {
    "org/gnome/desktop/input-sources".xkb-options = [ "caps:ctrl_modifier" ];
  };

  services.emacs = { package = pkgs.emacs-pgtk; };
}
