{
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
}
