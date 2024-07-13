{
  config,
  lib,
  pkgs,
  ...
}: {
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/37e728c9-3355-4449-b732-c5f00f4b5418";
    fsType = "ext4";
  };
  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/8759-6936";
    fsType = "vfat";
  };
  swapDevices = [];
  environment.variables = {
    # Enable rcom support even if the GPU is not officially supported anymore
    ROC_ENABLE_PRE_VEGA = "1";
  };
  hardware.amdgpu = {
    initrd.enable = true;
    amdvlk = {
      enable = true;
      support32Bit.enable = true;
      supportExperimental.enable = true;
    };
    opencl.enable = true;
  };
  nix.settings.max-jobs = lib.mkDefault 14;
}
