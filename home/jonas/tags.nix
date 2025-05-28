{
  config,
  systemConfig,
  pkgs,
  lib,
  ...
}: {
  home.sessionVariables = {
    GTAGSCONF = "${pkgs.global}/share/gtags/gtags.conf";
    GTAGSLABEL = "pygments";
    MAKEOBJDIRPREFIX = "/home/jonas/.cache/gtags";
  };
  systemd.user.tmpfiles.rules = [
    "D /home/jonas/.cache/gtags 0755 jonas jonas 7d -"
  ];
}
