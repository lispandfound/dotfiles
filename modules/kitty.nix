{ config, pkgs, ... }: {
  home.packages = with pkgs; [ kitty nerd-fonts.jetbrains-mono ];
  programs.kitty = {
    enable = true;
    settings = {
      allow_remote_control = true;
      enabled_layouts = "splits";
      shell = "zsh";
      font_family = "JetbrainsMono Nerd Font";
      font_size = 14.0;
      background = "#ffffff";
      foreground = "#262626";
      cursor = "#6fd2fc";
      selection_background = "#6fd2fc";
      color0 = "#000000";
      color8 = "#545753";
      color1 = "#f72729";
      color9 = "#fb0416";
      color2 = "#32895c";
      color10 = "#2cc631";
      color3 = "#f96f1c";
      color11 = "#fcd627";
      color4 = "#125ccf";
      color12 = "#156ffe";
      color5 = "#9f00bc";
      color13 = "#e800b0";
      color6 = "#32c2c0";
      color14 = "#39d5ce";
      color7 = "#b2b2b2";
      color15 = "#ededec";
      selection_foreground = "#ffffff";
    };
    keybindings = {
      "ctrl+left" = "neighboring_window left";
      "shift+left" = "move_window right";
      "ctrl+down" = "neighboring_window down";
      "shift+down" = "move_window up";
      "ctrl+right" = "neighboring_window right";
      "shift+right" = "move_window left";
      "ctrl+up" = "neighboring_window up";
      "shift+up" = "move_window down";
      "ctrl+y" = "launch --bias 30 --cwd current yazi";
      "ctrl+p" =
        "launch --location before --bias 20 --cwd current --title '*repl*' direnv exec . bpython";
      "ctrl+0" = ''
        launch --bias 30 fish -c "todo.sh ls | bat -p --paging=always --language=todo.txt"'';
    };
  };
}
