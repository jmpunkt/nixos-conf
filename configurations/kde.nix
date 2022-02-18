{ config
, pkgs
, ...
}:
{
  imports = [ ./desktop.nix ];
  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;
  environment.systemPackages =
    with pkgs;
    [ ark okular kate gwenview spectacle ktouch kdialog mpv smplayer libreoffice-qt krita skanlite pinentry_qt5 ];
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
