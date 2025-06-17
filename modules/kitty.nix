{ config, pkgs, ... }: {
  home.packages = with pkgs; [ kitty nerd-fonts.jetbrains-mono ];
  programs.kitty = {
    enable = true;
    settings = {
      allow_remote_control = true;
      enabled_layouts = "splits";
      shell = "zsh";
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
