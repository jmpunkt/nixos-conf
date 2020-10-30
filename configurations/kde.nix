{ config, pkgs, ... }:

{
  imports = [ ./desktop.nix ];

  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;

  environment.systemPackages = with pkgs; [
    kdeApplications.ark
    kdeApplications.okular
    kdeApplications.kate
    kdeApplications.gwenview
    kdeApplications.spectacle
    kdeApplications.ktouch
    kdeApplications.kdialog
    vlc
    libreoffice-qt
    krita
    skanlite
    pinentry_qt5
  ];

  services.xserver = {
    displayManager = {
      sddm.enable = true;
      autoLogin = {
        enable = true;
        user = "jonas";
      };
    };
    desktopManager.plasma5.enable = true;
  };
}
