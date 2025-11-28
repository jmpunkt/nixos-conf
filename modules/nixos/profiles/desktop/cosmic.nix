{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.desktop.cosmic;
in
{
  options.profiles.desktop.cosmic = {
    enable = lib.mkEnableOption "Cosmic desktop support";
  };

  config = lib.mkIf cfg.enable {
    profiles.desktop.enable = true;

    environment.systemPackages = with pkgs; [
      gparted
      fuzzel
    ];

    services.libinput = {
      enable = true;
      touchpad = {
        accelProfile = "flat";
        naturalScrolling = true;
        tapping = true;
      };
    };

    services.desktopManager.cosmic.enable = true;
    services.displayManager.cosmic-greeter.enable = true;
  };
}
