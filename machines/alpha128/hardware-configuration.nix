{ config
, lib
, pkgs
, ...
}:
{
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/37e728c9-3355-4449-b732-c5f00f4b5418";
    fsType = "ext4";
  };
  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/98C4-B2DF";
    fsType = "vfat";
  };
  swapDevices = [ ];
  nix.settings.max-jobs = lib.mkDefault 14;
}
