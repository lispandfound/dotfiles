{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    atool
    bat
    delta
    devenv
    direnv
    eza # A modern replacement for ‘ls’
    fd
    file
    fish
    fzf
    fzf # A command-line fuzzy finder
    gawk
    gh
    gnupg
    gnused
    gnutar
    jq # A lightweight and flexible command-line JSON processor
    p7zip
    ripgrep # recursively searches directories for a regex pattern
    starship
    tldr
    tree
    unzip
    todo-txt-cli
    watchexec
    which
    xz
    yazi
    zip
    zoxide
    zenith
    zstd
  ];
  programs.eza = {
    icons = "auto";
    enable = true;
    enableFishIntegration = true;
  };

  programs.bat = { enable = true; };

  programs.fzf = {
    enable = true;
    enableFishIntegration = false; # provided by the fish fzf plugin instead.
  };

  programs.starship = {
    enable = true;
    # custom settings
    settings = {
      add_newline = false;
      aws.disabled = true;
      gcloud.disabled = true;
      line_break.disabled = true;
      # General format configuration
      format =
        "$character$python[](fg:#c678dd bg:blue)$hostname$directory[](fg:blue bg:yellow)$git_branch$git_status[](fg:yellow) ";
      # Directory configuration
      directory = {
        format = "[  $path ]($style)";
        style = "fg:black bg:blue";
      };
      hostname = {
        format = "[ $ssh_symbol$hostname]($style)";
        ssh_symbol = "@";
        style = "fg:black bg:blue";
      };

      # Git branch configuration
      git_branch = {
        format = "[ $symbol$branch(:$remote_branch) ]($style)";
        symbol = " ";
        style = "fg:black bg:yellow";
      };

      # Git status configuration
      git_status = {
        format = "[$all_status]($style)";
        style = "fg:black bg:yellow";
      };

      # Mercurial (hg) branch configuration
      hg_branch = {
        format = "[ $symbol$branch ]($style)";
        symbol = " ";
      };

      # Command duration configuration
      cmd_duration = {
        format = "[  $duration ]($style)";
        style = "fg:bright-white bg:18";
      };

      # Character configuration (prompt symbols)
      character = {
        format = "$symbol";
        success_symbol =
          "[](fg:green)[󰌽 ](bg:green fg:black)[](fg:green bg:#c678dd )";
        error_symbol =
          "[](fg:red)[󰌽 ](bg:red fg:black)[](fg:red bg:#c678dd )";
      };

      # Python configuration
      python = {
        format = "[ $symbol$virtualenv $version ]($style)";
        style = "bg:#c678dd fg:black";
        symbol = " ";
      };
    };
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set -g fish_greeting ""
      set -g EDITOR "hx"
      fzf_configure_bindings
      function fzf-man
          if test -n "$argv[1]"
              man $argv
              return $status
          else
              man -k . | fzf --reverse --preview="echo {1,2} | sed 's/ (/./' | sed -E 's/\)\s*\$//' | xargs man" | awk '{print $1 "." $2}' | tr -d '()' | xargs -r man
              return $status
          end
      end
      function y
        	set tmp (mktemp -t "yazi-cwd.XXXXXX")
        	yazi $argv --cwd-file="$tmp"
        	if set cwd (command cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
        		builtin cd -- "$cwd"
        	end
        	rm -f -- "$tmp"
      end
    '';
    plugins = [
      {
        name = "fzf-fish";
        src = pkgs.fishPlugins.fzf-fish.src;
      }
      {
        name = "nix.fish";
        src = pkgs.fetchFromGitHub {
          owner = "kidonng";
          repo = "nix.fish";
          rev = "ad57d970841ae4a24521b5b1a68121cf385ba71e";
          sha256 =
            "13x3bfif906nszf4mgsqxfshnjcn6qm4qw1gv7nw89wi4cdp9i8q"; # You need to compute the SHA256 hash
        };
      }
    ];
    shellAbbrs = {
      rebuild = "sudo nixos-rebuild switch --flake ~/.dotfiles#$(hostname)";
      conf = "hx ~/.dotfiles";
      t = "todo.sh";
      cat = "bat";
      less = "bat --paging=always";
      tree = "eza --tree";
    };
  };

  programs.zoxide = {
    enable = true;
    options = [ "--cmd" "cd" ];
  };

}
