{ config, pkgs, ... }: {

  environment.systemPackages = with pkgs; [ davfs2 mount umount ];

  users.users.davfs2 = {
    isSystemUser = true;
    group = "davfs2";
  };
  security.wrappers = {
    mount.davfs = {
      source = "${pkgs.davfs2}/bin/mount.davfs";
      owner = "root";
      group = "root";
      permissions = "u+rx,g+rx,o+rx";
      setuid = true;
    };
  };
  fileSystems."/mnt/dufs" = {
    device = "https://media.tail7ee4b1.ts.net:5000";
    fsType = "davfs";
    options = [ "rw" "user" "noauto" ];
  };
  users.groups.davfs2 = { };
  environment.etc."davfs2/davfs2.conf".text = ''
    use_locks 0
    dav_group davfs2
  '';
}
