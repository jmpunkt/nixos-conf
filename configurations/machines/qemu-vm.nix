{
  pkgs,
  config,
  lib,
  ...
}:
{
  console = {
    font = "Lat2-Terminus16";
    keyMap = pkgs.lib.mkForce "us";
  };

  boot.kernelModules = [ "vfio-pci" ];
  boot.initrd.availableKernelModules = [
    "ata_piix"
    "uhci_hcd"
    "virtio_pci"
    "sr_mod"
    "virtio_blk"
  ];

  services.qemuGuest.enable = true;

  services.openssh = {
    ports = [ 11111 ];
    enable = true;
    settings = {
      PermitRootLogin = "yes";
    };
  };

  users = {
    mutableUsers = false;
    users.root = {
      password = "";
      isSystemUser = true;
    };
    users.jonas = {
      password = "";
    };
  };

  virtualisation = {
    memorySize = 8192;
    useBootLoader = false;
    msize = 16384 * 4;
    cores = 8;
    qemu = {
      package = pkgs.qemu_kvm;
      guestAgent.enable = true;
      options = [
        "-cpu host"
        "-M q35,smm=on,accel=kvm:tcg"

        "-object memory-backend-memfd,id=mem1,size=8G"
        "-machine memory-backend=mem1"

        # Enable Venus for 3D acceleration (https://wiki.cachyos.org/virtualization/virtio-venus/)
        "-vga none"
        "-display gtk,gl=on,show-cursor=on"
        "-device virtio-vga-gl,hostmem=4G,blob=true,venus=true"
      ];
    };
  };
}
