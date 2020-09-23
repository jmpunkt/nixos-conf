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

        The service must be manually started for each user with
        "systemctl --user start gamemode" or globally through
        <varname>services.gamemode.enable</varname>.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.gamemode = {
      Unit = { Description = pkgs.jmpunkt.gamemode.meta.description; };
      Install = { WantedBy = [ "default.target" ]; };
      Service = {
        Type = "dbus";
        BusName = "com.feralinteractive.GameMode";
        NotifyAccess = "main";
        ExecStart = "${pkgs.jmpunkt.gamemode}/bin/gamemoded";
      };
    };
  };
}
