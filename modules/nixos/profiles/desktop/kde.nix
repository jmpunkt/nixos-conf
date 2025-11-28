{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.desktop.kde;
in
{
  options.profiles.desktop.kde = {
    enable = lib.mkEnableOption "KDE desktop support";
  };

  config = lib.mkIf cfg.enable {
    profiles.desktop.enable = true;

    environment.systemPackages =
      with pkgs.kdePackages;
      [
        ark
        okular
        kate
        gwenview
        spectacle
        kdialog
        partitionmanager

      ]
      ++ (with pkgs; [ fuzzel ]);

    services.libinput = {
      enable = true;
      touchpad = {
        accelProfile = "flat";
        naturalScrolling = true;
        tapping = true;
      };
    };

    services.desktopManager.plasma6.enable = true;

    services.displayManager = {
      enable = true;
      sddm = {
        enable = true;
        wayland.enable = true;
      };
    };
  };
}
