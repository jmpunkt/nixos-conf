{ lib, pkgs, config, ... }:
with lib;
let cfg = config.services.gamemode;
in {
  options.services.gamemode = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to enable a user service of the Gamemode daemon. If <literal>true</literal>, <varname>services.emacs.install</varname> is considered <literal>true</literal>, whatever its value.
      '';
    };

    install = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to install a user service of the Gamemmode daemon. Once
        the service is started, use gamemoderun to connect to the
        daemon.
        The service must be manually started for each user with
        "systemctl --user start gamemode" or globally through
        <varname>services.gamemode.enable</varname>.
      '';
    };
  };

  config = mkIf (cfg.enable || cfg.install) {
    systemd.user.services.gamemode = {
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Type = "dbus";
        BusName = "com.feralinteractive.GameMode";
        NotifyAccess = "main";
        ExecStart = "${pkgs.jmpunkt.gamemode}/bin/gamemoded";
      };
    };
  };
}
