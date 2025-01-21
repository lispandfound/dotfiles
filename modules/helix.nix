{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ helix nixfmt-classic ];

  programs.helix = {
    enable = true;
    defaultEditor = true;
    settings = {
      theme = "onedark"; # Set the theme
      editor = {
        jump-label-alphabet =
          "aoeuidhtnsfygpcrlqjkbxmwvz"; # Set jump label alphabet
        cursor-shape = {
          insert = "bar"; # Cursor shape in insert mode
          normal = "block"; # Cursor shape in normal mode
          select = "underline"; # Cursor shape in select mode
        };
      };

      keys.normal = {
        V = [ "goto_first_nonwhitespace" "extend_to_line_end" ];
        D = [ "ensure_selections_forward" "extend_to_line_end" ];
        x = "extend_line";
        esc = [ "collapse_selection" "keep_primary_selection" ];
      };

      keys.normal."+" = {
        d = ":pipe doq --formatter numpy";
        q = ":pipe fmt -w 80 -p '#'";
        s = ":sh dashit-fzf-popup";
      };

      keys.select = {
        ";" = [ "collapse_selection" "normal_mode" ];
        x = "extend_line"; # Same behavior as normal mode x
        V = "extend_to_line_end"; # Same behavior as normal mode V
      };
    };
    languages.language-server = {
      tinymist = {
        command = "tinymist";
        config = { exportPdf = "onType"; };
      };

    };
    languages.language = [
      {
        name = "nix";
        auto-format = true;
        formatter.command = "${pkgs.nixfmt-classic}/bin/nixfmt";
      }
      {
        name = "markdown";
        auto-format = true;
        language-servers = [ "ltex-ls-plus" ];
      }
      {
        name = "typst";
        auto-format = true;
        language-servers = [ "tinymist" "ltex-ls-plus" ];
        formatter.command = "typstyle";
      }
    ];
  };
}
