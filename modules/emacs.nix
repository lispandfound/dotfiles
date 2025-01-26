{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ emacs30-pgtk hunspell hunspellDicts.en-au ];
}
