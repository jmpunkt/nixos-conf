{ config, pkgs, ... }:

{
  imports = [ ./base.nix ./locale.nix ./shell.nix ./tmux.nix ];

  # Use newer kernel for desktop based machines
  boot.kernelPackages = pkgs.linuxPackages_latest;

  environment.systemPackages = with pkgs; [
    alacritty
    cryptsetup
    gnupg
    feh
    thunderbird
    dbeaver
    keepassxc
    home-manager
    firefox

    discord
    tdesktop
    pencil

    ntfsprogs
    dosfstools
    xfsprogs.bin
    jfsutils
    f2fs-tools
    ntfs3g

    hunspell
    hunspellDicts.en-us-large
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
    symbola
  ];

  location = {
    latitude = 50.11;
    longitude = 8.682;
  };

  boot.supportedFilesystems =
    [ "btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];

  sound.enable = true;

  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };

    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  networking.networkmanager.enable = true;

  services = {
    redshift.enable = true;
    printing.enable = true;
    xserver.enable = true;
  };
}
