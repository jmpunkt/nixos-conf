{
  lib,
  pkgs,
  config,
  ...
}:
with lib; let
  cfg = config.services.gamemode;
in {
  options.services.gamemode = {
    enable =
      mkOption
      {
        type = types.bool;
        default = false;
        description = ''
          Whether to install a user service of the Gamemode daemon. This
          installs a daemon which listens for D-Bus events. It is not
          required to enable the daemon.
        '';
      };
    package =
      mkOption
      {
        type = types.package;
        default = pkgs.jmpunkt.gamemode;
      };
    ini =
      mkOption
      {
        type = types.lines;
        default = "";
        description = ''
          Configuration content for gamemode.
        '';
      };
  };
  config =
    mkIf
    cfg.enable
    {
      environment.systemPackages = [cfg.package];
      systemd.packages = [cfg.package];
      services.dbus.packages = [cfg.package];
      environment.etc."gamemode.ini".text = cfg.ini;
      security.pam.loginLimits = [
        {
          domain = "@gamemode";
          type = "-";
          item = "nice";
          value = "-10";
        }
      ];
    };
}
