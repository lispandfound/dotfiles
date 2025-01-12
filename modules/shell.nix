{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    fish
    fzf
    starship
    # common utilities    
    fd
    zip
    xz
    unzip
    p7zip
    ripgrep # recursively searches directories for a regex pattern
    jq # A lightweight and flexible command-line JSON processor
    eza # A modern replacement for ‘ls’
    fzf # A command-line fuzzy finder
    yazi
    file
    which
    tree
    gnused
    gnutar
    gawk
    zstd
    gnupg
    zoxide
    delta
    devenv
    direnv
    vault-tasks
  ];
  home.file.".config/vault-tasks/config.toml".source =
    ../config/vault-tasks/config.toml;

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
        format = "[ $path ]($style)";
        style = "fg:black bg:blue";
      };
      hostname = {
        format = "[ $ssh_symbol$hostname ]($style)";
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
      fzf_configure_bindings
      function __auto_source_venv --on-variable PWD --description "Activate/Deactivate virtualenv on directory change"
        status --is-command-substitution; and return

        # Check if we are inside a git directory
        if git rev-parse --show-toplevel &>/dev/null
          set gitdir (realpath (git rev-parse --show-toplevel))
          set cwd (pwd -P)
          # While we are still inside the git directory, find the closest
          # virtualenv starting from the current directory.
          while string match "$gitdir*" "$cwd" &>/dev/null
            if test -e "$cwd/.venv/bin/activate.fish"
              source "$cwd/.venv/bin/activate.fish" &>/dev/null 
              return
            else
              set cwd (path dirname "$cwd")
            end
          end
        end
        # If virtualenv activated but we are not in a git directory, deactivate.
        if test -n "$VIRTUAL_ENV"
          deactivate
        end
      end
      function fzf-man
          if test -n "$argv[1]"
              man $argv
              return $status
          else
              man -k . | fzf --reverse --preview="echo {1,2} | sed 's/ (/./' | sed -E 's/\)\s*\$//' | xargs man" | awk '{print $1 "." $2}' | tr -d '()' | xargs -r man
              return $status
          end
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
      t = "vault-tasks -v ~/.tasks";
    };
  };

  programs.zoxide = {
    enable = true;
    options = [ "--cmd" "cd" ];
  };

}
