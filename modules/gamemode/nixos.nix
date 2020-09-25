{ lib, pkgs, config, ... }:
with lib;
let cfg = config.services.gamemode;
in {
  options.services.gamemode = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to install a user service of the Gamemode daemon. This
        installs a daemon which listens for D-Bus events. It is not
        required to enable the daemon.
      '';
    };

    configFile = mkOption {
      type = types.string;
      default = "";
      description = ''
        Configuration file for gamemode.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      jmpunkt.gamemode
    ];

    security.pam.loginLimits = [{
      domain = "@gamemode";
      type = "-";
      item = "nice";
      value = "-10";
    }];

    systemd.user.services.gamemode = {
      description = pkgs.jmpunkt.gamemode.meta.description;
      serviceConfig = {
        Type = "dbus";
        BusName = "com.feralinteractive.GameMode";
        NotifyAccess = "main";
        ExecStart = "${pkgs.jmpunkt.gamemode}/bin/gamemoded -l";
      };
    };
  };
}
