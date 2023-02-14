{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./base.nix ./locale.nix ./shell.nix ./fish ./yubico.nix];
  boot = {
    # Use zen kernel for desktop based machines
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    supportedFilesystems = ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  };
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };
  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-kde
      ];
    };
  };
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [
    ntfsprogs
    dosfstools
    xfsprogs.bin
    jfsutils
    f2fs-tools
    ntfs3g
    chromium
    firefox-bin
    nyxt
    nix-tree
    mpv
  ];
  fonts = {
    fonts = with pkgs; [
      corefonts
      ibm-plex
      jetbrains-mono
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
    printing = {
      enable = true;
      drivers = with pkgs; [gutenprint hplip];
    };
    xserver = {
      enable = true;
      libinput = {
        enable = true;
        touchpad.accelProfile = "flat";
      };
    };
  };
}
