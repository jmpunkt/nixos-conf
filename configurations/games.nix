{ config, pkgs, ... }:

let
  steam = pkgs.unstable.steam.override {
    extraLibraries = pkgs: with config.hardware.opengl;
      if pkgs.hostPlatform.is64bit
      then [ package ] ++ extraPackages
      else [ package32 ] ++ extraPackages32;
  };
in
{
  environment.systemPackages = with pkgs; [
    minecraft
    steam
    steam.run
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
