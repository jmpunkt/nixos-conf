{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.profiles.desktop.wwm;
in
{
  config = lib.mkIf (cfg.enable && cfg.windowManager == "wayfire") {
    profiles.desktop.wwm.loginSession = lib.mkDefault (pkgs.writeShellScript "startwayfire" ''
      exec ${lib.getExe pkgs.wayfire}
    '');

    # TODO: Setup screencast settings: disable notifications, ...
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      config.common.default = "*";
      extraPortals = [
        pkgs.lxqt.xdg-desktop-portal-lxqt # file-picker
      ];
    };
  };
}
