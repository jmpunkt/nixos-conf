{
  lib,
  config,
  pkgs,
  options,
  ...
}: {
  imports = [
    ./minimal.nix
  ];
  boot.binfmt.emulatedSystems = ["aarch64-linux"];
  environment.systemPackages = with pkgs; [
    curl
    eza
    fd
    git
    hdparm
    htop
    inetutils
    pciutils
    ripgrep
    sdparm
    smartmontools
    srm
    unzip
    usbutils

    # filesystem utils
    dosfstools
    f2fs-tools
    jfsutils
    ntfs3g
    ntfsprogs
    xfsprogs.bin
  ];
  networking.firewall.enable = true;
  security.doas.enable = true;
  services.timesyncd.enable = lib.mkForce true;
  programs.fish.enable = true;
}
