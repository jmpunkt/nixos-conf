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
    desktopManager.plasma5.enable = true;
  };
  # enables auto-login and starts plasma wayland
  # NOTE: removes dependency on SDDM which currently required X11.
  services.greetd = let
    cmd = "dbus-run-session startplasma-wayland";
  in {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd \"${cmd}\"";
        user = "greeter";
      };
      initial_session = {
        command = "${cmd}";
        user = "jonas";
      };
    };
  };
}
