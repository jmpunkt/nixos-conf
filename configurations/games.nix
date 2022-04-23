{
  config,
  pkgs,
  lib,
  ...
}: let
  wine = pkgs.wineWowPackages.staging;
in {
  programs.firejail = {
    wrappedBinaries = {
      wine = {
        executable = "${lib.getBin wine}/bin/wine";
        profile = "${pkgs.firejail}/etc/firejail/wine.profile";
      };
      # lutris = {
      #   executable = "${lib.getBin pkgs.lutris}/bin/lutris";
      #   profile = "${pkgs.firejail}/etc/firejail/lutris.profile";
      # };
    };
  };
  programs.steam.enable = true;
  environment.systemPackages = with pkgs; [
    minecraft
    lutris
    samba
  ];
  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };
}
