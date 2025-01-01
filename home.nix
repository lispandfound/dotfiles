{ config, pkgs, ... }:

{
  home.username = "jake";
  home.homeDirectory = "/home/jake";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    helix
    python313
    fish
    starship
    kitty
    delta
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

    keepassxc
    syncthing

  ];

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
        github = {
          user = "lispandfound";
        };
        push = {
          default = "current";
        };
        pull = {
          rebase = false;
        };
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
        jump-label-alphabet = "aoeuidhtnsfygpcrlqjkbxmwvz";  # Set jump label alphabet
        # 25.01
        # end-of-line-diagnostics = "hint"; # Set end-of-line diagnostics level

        # inline-diagnostics = {
        #   cursor-line = "warning";  # Show warnings and errors on the cursor line inline
        # };

        cursor-shape = {
          insert = "bar";   # Cursor shape in insert mode
          normal = "block"; # Cursor shape in normal mode
          select = "underline"; # Cursor shape in select mode
        };
      };

      # Keybindings for normal mode
      keys.normal = {
        V = ["goto_first_nonwhitespace" "extend_to_line_end"];
        D = ["ensure_selections_forward" "extend_to_line_end"];
        x = "extend_line";
        # 25.01
        # W = "@miw";
        esc = ["collapse_selection" "keep_primary_selection"];
      };

      # Keybindings for "+" in normal mode
      keys.normal."+" = {
        d = ":pipe doq --formatter numpy";
        q = ":pipe fmt -w 80 -p '#'";
        s = ":sh dashit-fzf-popup";
      };

      # Keybindings for select mode
      keys.select = {
        ";" = ["collapse_selection" "normal_mode"];
        x = "extend_line"; # Same behavior as normal mode x
        V = "extend_to_line_end"; # Same behavior as normal mode V
      };
    };
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
      format = "$character$python[](fg:#c678dd bg:blue)$directory[](fg:blue bg:yellow)$git_branch$git_status[](fg:yellow) ";
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
        success_symbol = "[](fg:green)[󰌽 ](bg:green fg:black)[](fg:green bg:#c678dd)";
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
    '';
    shellAbbrs = {
      rebuild = "sudo nixos-rebuild switch --flake ~/.dotfiles#laptop";
      conf = "hx ~/.dotfiles";
    };
  };


  programs.zoxide = {
    enable = true;
    options = ["--cmd" "cd"];
  };

  programs.kitty = {
    enable = true;
    settings = {
      allow_remote_control = true;
      enabled_layouts = "splits";
      wayland_titlebar_color = "#000000";
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


      active_tab_foreground   = "#282c34";
      active_tab_background   = "#979eab";
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


  services.syncthing = {
    enable = true;
  };
}
