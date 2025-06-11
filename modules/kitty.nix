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
      background = "#fafafa";
      foreground = "#5b6673";
      cursor = "#ff6900";
      selection_background = "#f0ede4";
      color0 = "#000000";
      color8 = "#323232";
      color1 = "#ff3333";
      color9 = "#ff6565";
      color2 = "#86b200";
      color10 = "#b8e532";
      color3 = "#f19618";
      color11 = "#ffc849";
      color4 = "#41a6d9";
      color12 = "#73d7ff";
      color5 = "#f07078";
      color13 = "#ffa3aa";
      color6 = "#4cbe99";
      color14 = "#7ff0cb";
      color7 = "#ffffff";
      color15 = "#ffffff";
      selection_foreground = "#fafafa";
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
