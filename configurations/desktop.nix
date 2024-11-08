{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop-minimal.nix];

  # scanner
  hardware = {
    sane.enable = true;
    sane.extraBackends = with pkgs; [hplipWithPlugin];
    sane.disabledDefaultBackends = ["escl"];
  };

  # sound
  hardware.pulseaudio.enable = lib.mkForce false;
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
  };

  # printing
  services = {
    printing.enable = true;
    printing.drivers = with pkgs; [hplip];
    avahi = {
      enable = true;
      nssmdns4 = true;
      nssmdns6 = true;
    };
  };

  # dns
  networking.nameservers = [
    # Quad9
    "9.9.9.9"
    "149.112.112.112"
    "2620:fe::fe"
    "2620:fe::9"
  ];
  services.resolved = {
    enable = true;
    dnssec = "true";
    dnsovertls = "true";
    fallbackDns = [
      # Cloudflare
      "1.1.1.1"
      "1.0.0.1"
      "2606:4700:4700::1111"
      "2606:4700:4700::1001"
    ];
  };

  programs = {
    chromium.enable = true;
    # TODO: only available in unstable
    # thunderbird.enable = true;
  };
  environment.systemPackages = with pkgs; [
    # gui
    audacious
    discord
    tdesktop
    thunderbird
    chatterino2
    mpv

    # cli
    streamlink
    yt-dlp
  ];
}
