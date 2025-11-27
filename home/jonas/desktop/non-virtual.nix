{
  config,
  systemConfig,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./minimal.nix
  ];
  programs.chromium.enable = true;
  # programs.thunderbird = {
  #   enable = true;
  #   profiles.main = {
  #     isDefault = true;
  #     withExternalGnupg = true;
  #   };
  # };
  programs.mpv.enable = true;
  home.packages = with pkgs; [
    # gui
    thunderbird
    audacious
    discord
    telegram-desktop
    chatterino2

    # cli
    streamlink
    yt-dlp
  ];
  programs.keepassxc = {
    enable = true;
    settings = {
      Browser.Enabled = true;
    };
  };
  services.syncthing = {
    enable = true;
    tray.enable = true;
  };
}
