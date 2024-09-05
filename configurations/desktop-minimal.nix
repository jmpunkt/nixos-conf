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
  programs = {
    git.enable = true;
    firefox = {
      enable = true;
      package = pkgs.firefox-bin;
    };
  };
  environment.systemPackages = with pkgs; [
    # gui
    binutils-unwrapped
    gnupg
    keepassxc

    # cli
    hyperfine
    nix-tree
    sqlite
    tokei

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
      dejavu_fonts
      freefont_ttf
      liberation_ttf
      noto-fonts-monochrome-emoji
      # Monospace
      fantasque-sans-mono
      # Emacs Icons
      emacs-all-the-icons-fonts
    ];
    fontconfig.defaultFonts.monospace = ["Jetbrains Mono"];
  };
  hardware = {
    opengl.enable = true;
  };
  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkForce false;
  systemd.services.NetworkManager-wait-online.enable = false;
}
