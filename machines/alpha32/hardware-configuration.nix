# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/0cac9eee-b9d8-410a-8a42-ac0bd14b9423";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."enc-root".device =
    "/dev/disk/by-uuid/ed91ad53-bdf5-4c16-9581-2bd6120c4ede";

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/8BB2-DEE5";
    fsType = "vfat";
  };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
