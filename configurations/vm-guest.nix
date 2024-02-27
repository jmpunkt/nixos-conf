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
      package = pkgs.qemu;
      options = [
        "-vga virtio"
        "-enable-kvm"
        "-machine q35"
        "-display sdl"
      ];
    };
  };
}
