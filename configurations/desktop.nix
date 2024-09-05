{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop-minimal.nix];
  hardware = {
    sane.enable = true;
    sane.extraBackends = with pkgs; [hplipWithPlugin];
    sane.disabledDefaultBackends = ["escl"];
    pulseaudio.enable = lib.mkForce false;
  };
  sound.enable = lib.mkForce false;
  security.rtkit.enable = true;
  services = {
    pipewire = {
      enable = true;
      pulse = {enable = true;};
      alsa = {
        enable = true;
        support32Bit = true;
      };
    };
    printing.enable = true;
    printing.drivers = with pkgs; [hplip];
    avahi = {
      enable = true;
      nssmdns4 = true;
      nssmdns6 = true;
    };
    unbound.enable = true;
    nscd.enableNsncd = true;
  };
  location = {
    latitude = 50.11;
    longitude = 8.682;
  };
  programs = {
    chromium.enable = true;
    thunderbird.enable = true;
  };
  environment.systemPackages = with pkgs; [
    # gui
    audacious
    discord
    tdesktop
    streamlink
    yt-dlp
    chatterino2
    mpv

    # cli
    streamlink
  ];
}
