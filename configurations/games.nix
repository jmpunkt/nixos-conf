{ config
, pkgs
, ...
}:
{
  programs.steam.enable = true;
  environment.systemPackages = with pkgs; [ minecraft ];
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
