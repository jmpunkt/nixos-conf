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
  environment.systemPackages = with pkgs; [
    # gui
    binutils-unwrapped
    firefox-bin
    gnupg
    mpv
    jmpunkt.emacs

    # cli
    git
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
      roboto
      noto-fonts
      freefont_ttf
      liberation_ttf
      # Monospace
      ibm-plex
      jetbrains-mono
      fantasque-sans-mono
      cascadia-code
      # Emacs Icons
      emacs-all-the-icons-fonts
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
  networking.wireless.enable = lib.mkForce false;
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
      nssmdns4 = true;
      nssmdns6 = true;
    };
  };
}
