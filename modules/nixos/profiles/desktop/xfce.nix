{
  config,
  lib,
  ...
}:

let
  cfg = config.profiles.desktop.xfce;
in
{
  options.profiles.desktop.xfce = {
    enable = lib.mkEnableOption "XFCE desktop support";
  };

  config = lib.mkIf cfg.enable {
    profiles.desktop.enable = true;

    services.xserver = {
      enable = true;
      desktopManager.xfce.enable = true;
    };

    services.displayManager = {
      defaultSession = "xfce";
    };
  };
}
