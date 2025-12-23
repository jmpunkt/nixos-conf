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
  config = lib.mkIf (cfg.enable && cfg.windowManager == "hyprland") {
    programs.hyprland.enable = true;
    profiles.desktop.wwm.loginSession = lib.mkDefault "${config.programs.hyprland.package}/bin/Hyprland";
  };
}
