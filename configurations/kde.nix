{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop.nix];
  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;
  environment.systemPackages = with pkgs; [
    ark
    okular
    kate
    gwenview
    spectacle
    ktouch
    kdialog
    libreoffice
    krita
    skanlite
    pinentry-qt
    partition-manager
  ];
  services.xserver = {
    # + WAYLAND
    desktopManager.plasma5.runUsingSystemd = true;
    displayManager.sessionPackages = [
      (pkgs.plasma-workspace.overrideAttrs
        (old: {passthru.providedSessions = ["plasmawayland"];}))
    ];
    # - WAYLAND
    displayManager = {
      sddm.enable = true;
      defaultSession = "plasmawayland";
      autoLogin = {
        enable = true;
        user = "jonas";
      };
    };
    desktopManager.plasma5.enable = true;
  };
}
