{
  pkgs,
  config,
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
    writableStoreUseTmpfs = false;
    msize = 16384 * 4;
    cores = 4;
    qemu = {
      package = pkgs.qemu_kvm;
      guestAgent.enable = true;
      options = [
        "-enable-kvm"
        "-cpu host"
        # normal
        "-vga virtio"
        # "-display sdl,gl=on,show-cursor=off"
      ];
    };
  };
}
