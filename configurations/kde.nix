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
    pinentry-qt
    partition-manager
  ];

  xdg.portal = {
    extraPortals = with pkgs; [
      xdg-desktop-portal-kde
    ];
  };

  services.xserver = {
    enable = true;
    libinput = {
      enable = true;
      touchpad.accelProfile = "flat";
    };

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
