# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  config,
  lib,
  pkgs,
  ...
}:
{
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "usb_storage"
    "sd_mod"
    "rtsx_pci_sdmmc"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4c9ac133-ebe9-4a36-93cd-c5af2be2c971";
    fsType = "ext4";
  };
  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/18484e19-3cbf-4c75-9dfc-cc74f9d2caf2";
  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/ED98-D564";
    fsType = "vfat";
  };
  swapDevices = [ ];
  nix.settings.max-jobs = lib.mkDefault 8;
}
