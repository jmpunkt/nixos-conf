{
  lib,
  config,
  pkgs,
  options,
  ...
}:
{
  imports = [
    ./barebones.nix
  ];
  environment.systemPackages = with pkgs; [
    curl
    wget
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
}
