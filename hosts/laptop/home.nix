{ config, pkgs, ... }:

{
  imports = [ ../../modules/helix.nix ../../modules/shell.nix ];
  home.username = "jake";
  home.homeDirectory = "/home/jake";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    kitty
    git
    gitu

    # archives

    # utils

    nerd-fonts.jetbrains-mono

    keepassxc
    syncthing

  ];

  programs.gnome-shell = {
    enable = true;
    extensions = [
      { package = pkgs.gnomeExtensions.gsconnect; }
      { package = pkgs.gnomeExtensions.hide-minimized; }
    ];
  };

  # basic configuration of git, please change to your own
  programs.git = {
    enable = true;
    userName = "Jake Faulkner";
    userEmail = "jakefaulkn@gmail.com";
    delta = {
      enable = true;
      options = {
        navigate = true;
        side-by-side = true;
      };
    };
    extraConfig = {
      github = { user = "lispandfound"; };
      push = { default = "current"; };
      pull = { rebase = false; };
      merge = {
        conflictstyle = "diff3";
        tool = "vimdiff";
      };
    };
  };

  # starship - an customizable prompt for any shell

  programs.kitty = {
    enable = true;
    settings = {
      allow_remote_control = true;
      enabled_layouts = "splits";
      wayland_titlebar_color = "#282c34";
      font_family = "JetbrainsMono Nerd Font";
      foreground = "#979eab";
      background = "#282c34";

      color0 = "#282c34";
      color1 = "#e06c75";
      color2 = "#98c379";
      color3 = "#e5c07b";
      color4 = "#61afef";
      color5 = "#be5046";
      color6 = "#56b6c2";
      color7 = "#979eab";
      color8 = "#393e48";
      color9 = "#d19a66";
      color10 = "#56b6c2";
      color11 = "#e5c07b";
      color12 = "#61afef";
      color13 = "#be5046";
      color14 = "#56b6c2";
      color15 = "#abb2bf";

      active_tab_foreground = "#282c34";
      active_tab_background = "#979eab";
      inactive_tab_foreground = "#abb2bf";
      inactive_tab_background = "#282c34";
    };
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
