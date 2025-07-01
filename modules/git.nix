{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    gitu
    git
    git-lfs
    delta
    meld
    wl-clipboard
    jujutsu
  ];

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
    ignores = [ ".jj" ];
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
  programs.jujutsu.settings = {
    user = {
      email = "jakefaulkn@gmail.com";
      name = "Jake Faulkner";
    };
  };
}
