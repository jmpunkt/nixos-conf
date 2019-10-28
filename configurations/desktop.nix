{ config, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./locale.nix
    ./shell.nix
    ./tmux.nix
  ];

  environment.systemPackages = with pkgs; [
    alacritty
    cryptsetup
    gnupg
    feh
    thunderbird
    dbeaver
    keepassxc
    home-manager
    # !! use the firefox version distrbuted by Mozilla
    firefox-bin

    discord
    tdesktop
    pencil

    ntfsprogs
    dosfstools
    xfsprogs.bin
    jfsutils
    f2fs-tools
    ntfs3g

    hunspellDicts.en-gb-ize
    hunspellDicts.en-us
    hunspellDicts.de-de

    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
  ];

  fonts.fonts = with pkgs; [
    corefonts
    dejavu_fonts
    ibm-plex
    fira
    fira-code
    fira-mono
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
    roboto
    roboto-mono
    roboto-slab
    monoid

    # Emacs Icons
    emacs-all-the-icons-fonts
  ];

  location = {
    latitude = 50.110;
    longitude = 8.682;
  };

  boot.supportedFilesystems = [ "btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  networking.networkmanager.enable = true;

  services.redshift.enable = true;
  services.printing.enable = true;
  services.xserver.enable = true;
  services.xserver.layout = "de";
}
