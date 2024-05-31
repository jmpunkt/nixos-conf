{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop.nix];

  environment.systemPackages = with pkgs; [
    ark
    okular
    kate
    gwenview
    spectacle
    ktouch
    kdialog
    skanlite
    partition-manager
  ];

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
    autoLogin = {
      enable = true;
      user = "jonas";
    };
  };
}
