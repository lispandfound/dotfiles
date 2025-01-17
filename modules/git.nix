{ config, pkgs, ... }: {
  home.packages = with pkgs; [ gitu git delta meld wl-clipboard ];

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
      github = { user = "lispandfound"; };
      push = { default = "current"; };
      pull = { rebase = false; };
      merge = {
        conflictstyle = "diff3";
        tool = "meld";
      };
      mergetool."meld" = {
        cmd = ''meld "$LOCAL" "$MERGED" "$REMOTE" --output "$MERGED"'';
      };
    };
  };
}
