{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop.nix];
  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;
  environment.systemPackages = with pkgs; [ark okular kate gwenview spectacle mpv smplayer ktouch kdialog libreoffice krita skanlite pinentry_qt5];
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
