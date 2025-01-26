{ inputs, pkgs, ... }:

{
  home.packages = [
    inputs.helix.packages."${pkgs.system}".helix
    pkgs.nixfmt-classic
    inputs.scls.defaultPackage.${pkgs.system}
  ];
  home.file.".config/helix/external-snippets.toml".text = ''
    [[sources]] # list of sources to load
    name = "friendly-snippets"  # optional name shown on snippet description
    git = "https://github.com/rafamadriz/friendly-snippets.git" # git repo with snippets collections
    [[sources.paths]] # list of paths to load on current source
    scope = ["python"]  # optional scopes for current snippets
    path = "snippets/python/python.json"  # where snippet file or dir located in repo
    [[sources.paths]] # list of paths to load on current source
    scope = ["git-commit"]  # optional scopes for current snippets
    path = "snippets/gitcommit.json"  # where snippet file or dir located in repo
  '';

  programs.helix = {
    enable = true;
    package = inputs.helix.packages."${pkgs.system}".helix;
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

      scls = {
        command = "simple-completion-language-server";
        config = {
          max_completion_items = 100;
          feature_words = true;
          feature_snippets = true;
          snippets_first = true;
          snippets_inline_by_word_tail = false;
          feature_unicode_input = false;
          feature_paths = false;
          feature_citations = false;
        };
      };

    };
    languages.language = [
      {
        name = "nix";
        auto-format = true;
        formatter.command = "${pkgs.nixfmt-classic}/bin/nixfmt";
      }
      {
        name = "python";
        auto-format = true;
        language-servers = [
          "scls"
          {
            name = "pylsp";
            except-features = [ "format" ];
          }
          "ruff"
        ];
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
      {
        name = "git-commit";
        auto-format = true;
        language-servers = [ "scls" ];
      }
    ];
  };
}
