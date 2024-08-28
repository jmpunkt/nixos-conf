{
  pkgs,
  config,
  ...
}: {
  console = {
    font = "Lat2-Terminus16";
    keyMap = "de";
  };

  boot.kernelModules = ["vfio-pci"];

  services.qemuGuest.enable = true;

  services.openssh = {
    ports = [11111];
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
    cores = 16;
    qemu = {
      package = pkgs.qemu_kvm;
      guestAgent.enable = true;
      options = [
        "-enable-kvm"

        # normal
        "-vga virtio"
        "-display sdl,gl=on,show-cursor=off"
      ];
    };
  };
}
