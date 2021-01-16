{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    steam
    minecraft
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
