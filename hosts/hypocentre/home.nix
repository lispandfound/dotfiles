{ config, pkgs, ... }:

{
  home.username = "jfa92";
  home.homeDirectory = "/home/jfa92";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    bash
    devenv
    helix
    python313
    fish
    starship
    delta
    fd
    git
    gitu

    # archives
    zip
    xz
    unzip
    p7zip

    # utils
    ripgrep # recursively searches directories for a regex pattern
    jq # A lightweight and flexible command-line JSON processor
    eza # A modern replacement for ‘ls’
    fzf # A command-line fuzzy finder
    yazi
    nixfmt-classic

    # misc
    cowsay
    file
    which
    tree
    gnused
    gnutar
    gawk
    zstd
    gnupg
    zoxide

    nerd-fonts.jetbrains-mono

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
    userEmail = "jfa92faulkn@gmail.com";
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

  programs.helix = {
    enable = true;
    defaultEditor = true;
    settings = {
      theme = "onedark"; # Set the theme

      editor = {
        jump-label-alphabet =
          "aoeuidhtnsfygpcrlqjkbxmwvz"; # Set jump label alphabet
        # 25.01
        # end-of-line-diagnostics = "hint"; # Set end-of-line diagnostics level

        # inline-diagnostics = {
        #   cursor-line = "warning";  # Show warnings and errors on the cursor line inline
        # };

        cursor-shape = {
          insert = "bar"; # Cursor shape in insert mode
          normal = "block"; # Cursor shape in normal mode
          select = "underline"; # Cursor shape in select mode
        };
      };

      # Keybindings for normal mode
      keys.normal = {
        V = [ "goto_first_nonwhitespace" "extend_to_line_end" ];
        D = [ "ensure_selections_forward" "extend_to_line_end" ];
        x = "extend_line";
        # 25.01
        # W = "@miw";
        esc = [ "collapse_selection" "keep_primary_selection" ];
      };

      # Keybindings for "+" in normal mode
      keys.normal."+" = {
        d = ":pipe doq --formatter numpy";
        q = ":pipe fmt -w 80 -p '#'";
        s = ":sh dashit-fzf-popup";
      };

      # Keybindings for select mode
      keys.select = {
        ";" = [ "collapse_selection" "normal_mode" ];
        x = "extend_line"; # Same behavior as normal mode x
        V = "extend_to_line_end"; # Same behavior as normal mode V
      };
    };
    languages.language = [{
      name = "nix";
      auto-format = true;
      formatter.command = "${pkgs.nixfmt-classic}/bin/nixfmt";
    }];

  };

  programs.fzf = {
    enable = true;
    enableFishIntegration = false; # provided by the fish fzf plugin instead.
  };

  # starship - an customizable prompt for any shell
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
        "$character$python[](fg:#c678dd bg:blue)$directory[](fg:blue bg:yellow)$git_branch$git_status[](fg:yellow) ";
      # Directory configuration
      directory = {
        format = "[  $path ]($style)";
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
          "[](fg:green)[󰌽 ](bg:green fg:black)[](fg:green bg:#c678dd)";
        error_symbol = "[](fg:red)[󰌽 ](bg:red fg:black)[](fg:red bg:#c678dd)";
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
    '';
    plugins = [{
      name = "fzf-fish";
      src = pkgs.fishPlugins.fzf-fish.src;
    }];
    shellAbbrs = {
      rebuild = "sudo nixos-rebuild switch --flake ~/.dotfiles#$(hostname)";
      conf = "hx ~/.dotfiles";
    };
  };

  programs.zoxide = {
    enable = true;
    options = [ "--cmd" "cd" ];
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

}
