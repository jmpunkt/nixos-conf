{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop.nix];
  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;
  environment.systemPackages = with pkgs; [ark okular kate gwenview spectacle mpv smplayer ktouch kdialog libreoffice krita skanlite pinentry-qt partition-manager];
  programs.firejail = {
    enable = true;
    wrappedBinaries = {
      # mpv = {
      #   executable = "${lib.getBin pkgs.mpv}/bin/mpv";
      #   profile = "${pkgs.firejail}/etc/firejail/mpv.profile";
      # };
      # smplayer = {
      #   executable = "${lib.getBin pkgs.smplayer}/bin/smplayer";
      #   profile = "${pkgs.firejail}/etc/firejail/smplayer.profile";
      # };
    };
  };
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
