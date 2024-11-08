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
    unbound.enable = true;
    nscd.enableNsncd = true;
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
