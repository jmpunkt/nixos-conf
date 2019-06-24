{ config, pkgs, ... }:

{
  imports = [ ./desktop.nix ];

  environment.systemPackages = with pkgs; [
    ark
    vlc
    okular
    libreoffice-fresh
    krita
    inkscape
    kate
    gwenview
    kdeApplications.spectacle
    kdeApplications.ktouch
    kdeApplications.kdialog
    skanlite
    kgpg
    pinentry_qt5
  ];

  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = true;
  services.xserver.displayManager.sddm.autoLogin.user = "jonas";
  services.xserver.desktopManager.plasma5.enable = true;
}
