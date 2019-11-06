{ config, pkgs, ... }:

{
  imports = [ ./desktop.nix ];

  environment.systemPackages = with pkgs; [
    ark
    vlc
    okular
    libreoffice-fresh
    krita
    kate
    gwenview
    kdeApplications.spectacle
    kdeApplications.ktouch
    kdeApplications.kdialog
    skanlite
    kgpg
    pinentry_qt5
  ];

  services.xserver = {
    displayManager.sddm = {
      enable = true;
      autoLogin = {
        enable = true;
        user = "jonas";
      };
    };
    desktopManager.plasma5.enable = true;
  };
}
