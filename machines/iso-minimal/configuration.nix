{
  modulesPath,
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
    ../../configurations/minimal.nix
  ];
  boot.supportedFilesystems = lib.mkForce ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  isoImage.volumeID = "nixos-${config.system.nixos.release}-${pkgs.stdenv.hostPlatform.uname.processor}";
  environment.systemPackages = with pkgs; [neovim git];
  networking.useDHCP = true;
  services.timesyncd.enable = true;
  time.timeZone = "Europe/Berlin";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  services.xserver.xkb.layout = "de";
  console.keyMap = "de";
}
