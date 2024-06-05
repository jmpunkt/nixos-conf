{
  config,
  pkgs,
  lib,
  ...
}: {
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };
  environment.systemPackages = with pkgs; [
    # gui
    audacious
    chromium
    discord
    krita
    tdesktop
    thunderbird
    streamlink
    yt-dlp
    chatterino2

    # cli
    streamlink
  ];
}
