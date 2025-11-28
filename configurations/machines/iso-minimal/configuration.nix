{
  modulesPath,
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
  ];
  profiles = {
    locales.germany.enable = true;
  };
  boot.supportedFilesystems = lib.mkForce [
    "btrfs"
    "reiserfs"
    "vfat"
    "f2fs"
    "xfs"
    "ntfs"
    "cifs"
  ];
  isoImage.volumeID = "nixos-${config.system.nixos.release}-${pkgs.stdenv.hostPlatform.uname.processor}";
  environment.systemPackages = with pkgs; [
    neovim
    git
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
