{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./base.nix ./locale.nix ./shell.nix ./fish ./yubico.nix];
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  };
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };
  xdg.portal.enable = true;
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [
    # gui
    audacious
    binutils-unwrapped
    chromium
    discord
    firefox-bin
    gnupg
    krita
    mpv
    tdesktop
    thunderbird
    streamlink
    yt-dlp
    jmpunkt.chatterino2-nigthly
    jmpunkt.emacs

    # cli
    git
    hyperfine
    nix-tree
    sqlite
    tokei
    streamlink

    # spelling
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    enchant
    hunspell
    hunspellDicts.en-us-large
    jmpunkt.hunspellDicts.de-de
  ];
  fonts = {
    packages = with pkgs; [
      corefonts
      # ibm-plex
      jetbrains-mono
      fantasque-sans-mono
      cascadia-code
      # Emacs Icons
      emacs-all-the-icons-fonts
      symbola
    ];
    fontconfig.defaultFonts.monospace = ["Jetbrains Mono"];
  };
  location = {
    latitude = 50.11;
    longitude = 8.682;
  };
  sound.enable = lib.mkForce false;
  hardware = {
    sane.enable = true;
    opengl.enable = true;
    pulseaudio.enable = lib.mkForce false;
  };
  networking.networkmanager.enable = true;
  systemd.services.NetworkManager-wait-online.enable = false;
  security.rtkit.enable = true;
  services = {
    unbound.enable = true;
    nscd.enableNsncd = true;
    pipewire = {
      enable = true;
      pulse = {enable = true;};
      alsa = {
        enable = true;
        support32Bit = true;
      };
    };
    printing.enable = true;
    avahi = {
      enable = true;
      nssmdns = true;
    };
  };
}
