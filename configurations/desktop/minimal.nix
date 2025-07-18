{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../tools.nix
    ../locale.nix
  ];
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  };
  services.scx.enable = true;
  services.scx.scheduler = "scx_rusty";
  # Automatic garbage collection (system profiles)
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 14d";
    randomizedDelaySec = "5min";
  };

  fonts = {
    packages = with pkgs; [
      dejavu_fonts
      freefont_ttf
      liberation_ttf
      noto-fonts
      noto-fonts-emoji
      # Monospace
      fantasque-sans-mono
    ];
  };
  hardware. graphics.enable = true;
  location = {
    latitude = 50.11;
    longitude = 8.682;
  };
  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkForce false;
  systemd.services.NetworkManager-wait-online.enable = false;
  # Remove logs older than 3 days.
  services.journald.extraConfig = ''
    MaxFileSec=3day
  '';
}
