{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.desktop.hyprland;
in
{
  options.profiles.desktop.hyprland = {
    enable = lib.mkEnableOption "Hyprland desktop support";
  };

  config = lib.mkIf cfg.enable {
    profiles.desktop.enable = true;
    programs.hyprland.enable = true;
    security.polkit.enable = true;
    services.blueman.enable = if config.hardware.bluetooth.enable then true else false;
    security.pam.services.swaylock = { };
    services.greetd =
      let
        cmd = "${config.programs.hyprland.finalPackage}/bin/Hyprland";
      in
      {
        enable = true;
        settings = {
          default_session = {
            command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd \"${cmd}\"";
            user = "greeter";
          };
          initial_session = {
            command = "${cmd}";
            user = config.services.displayManager.autoLogin.user;
          };
        };
      };
  };
}
