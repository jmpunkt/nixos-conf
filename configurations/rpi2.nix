{
  modulesPath,
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    "${modulesPath}/profiles/barebones.nix"
    "${modulesPath}/installer/sd-card/sd-image.nix"
    ./minimal-tools.nix
    ./sd-rpi2.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot.loader.grub.enable = lib.mkForce false;
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.supportedFilesystems = lib.mkForce [
    "vfat"
    "f2fs"
  ];

  boot.consoleLogLevel = lib.mkDefault 7;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # The serial ports listed here are:
  # - ttyS0: for Tegra (Jetson TK1)
  # - ttymxc0: for i.MX6 (Wandboard)
  # - ttyAMA0: for Allwinner (pcDuino3 Nano) and QEMU's -machine virt
  # - ttyO0: for OMAP (BeagleBone Black)
  # - ttySAC2: for Exynos (ODROID-XU3)
  boot.kernelParams = [
    "console=ttyS0,115200n8"
    "console=ttymxc0,115200n8"
    "console=ttyAMA0,115200n8"
    "console=ttyO0,115200n8"
    "console=ttySAC2,115200n8"
    "console=tty0"
  ];

  system.disableInstallerTools = true;

  sdImage = {
    populateFirmwareCommands =
      let
        configTxt = pkgs.writeText "config.txt" ''
          # Prevent the firmware from smashing the framebuffer setup done by the mainline kernel
          # when attempting to show low-voltage or overtemperature warnings.
          avoid_warnings=1

          [pi2]
          kernel=u-boot-rpi2.bin

          # U-Boot used to need this to work, regardless of whether UART is actually used or not.
          # TODO: check when/if this can be removed.
          enable_uart=1
        '';
      in
      ''
        (cd ${pkgs.raspberrypifw}/share/raspberrypi/boot && cp bootcode.bin fixup*.dat start*.elf $NIX_BUILD_TOP/firmware/)
        cp ${pkgs.ubootRaspberryPi2}/u-boot.bin firmware/u-boot-rpi2.bin
        cp ${configTxt} firmware/config.txt
      '';
    populateRootCommands = ''
      mkdir -p ./files/boot
      ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
    '';
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  services.timesyncd.enable = true;

  users.mutableUsers = false;

  nix.settings.allowed-users = [ "root" ];
  nixpkgs.config.allowUnsupportedSystem = true;

  documentation.enable = lib.mkForce false;
  documentation.man.enable = lib.mkForce false;
  documentation.man.man-db.enable = lib.mkForce false;
  documentation.man.man-db.package = lib.mkForce pkgs.hello;
  documentation.info.enable = lib.mkForce false;
  documentation.nixos.enable = lib.mkForce false;

  services.udisks2.enable = false;
  security.polkit.enable = false;
  hardware.bluetooth.enable = false;

  nix.settings = {
    substituters = [
      "https://nix-community.cachix.org"
      "https://thefloweringash-armv7.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "thefloweringash-armv7.cachix.org-1:v+5yzBD2odFKeXbmC+OPWVqx4WVoIVO6UXgnSAWFtso="
    ];
  };
}
