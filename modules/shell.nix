{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    aider-chat
    atool
    bat
    bottom
    cargo
    copilot-node-server
    delta
    devenv
    direnv
    dtach
    eza # A modern replacement for ‘ls’
    fd
    file
    zsh
    grml-zsh-config
    fzf # A command-line fuzzy finder
    gawk
    gcc
    gh
    gnupg
    gnused
    gnutar
    jq # A lightweight and flexible command-line JSON processor
    nil
    nodejs
    p7zip
    pandoc
    ripgrep # recursively searches directories for a regex pattern
    rustc
    starship
    tldr
    todo-txt-cli
    tree
    unzip
    watchexec
    which
    xan
    xz
    yazi
    zenith
    zip
    zoxide
    zstd
    zsh
  ];

  programs.eza = {
    icons = "auto";
    enable = true;
    enableZshIntegration = true;
  };

  programs.bat = {
    enable = true;

  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    options = [ "--cmd" "cd" ];
  };
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    initContent = lib.mkBefore ''
      # Load grml config first
      source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    '';
    shellAliases = {
      ssh = "kitten ssh";
      rebuild =
        ''sudo nixos-rebuild switch --flake "$HOME/.dotfiles#$(hostname)"'';
      cat = "bat";
      conf = "hx ~/.dotfiles";
      less = "bat --paging=always";
    };
  };

}
