{ config, pkgs, options, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/profiles/hardened.nix>
    <nixpkgs/nixos/modules/profiles/minimal.nix>
    ./hardware-configuration.nix
    ./services.nix
    ./../../configurations/base.nix
    ./../../configurations/locale.nix
    ./../../configurations/shell.nix
    ./../../configurations/users/admin.nix
  ];

  nix.nixPath = options.nix.nixPath.default ++
                [ "nixpkgs-overlays=/etc/nixos/nixos-conf/overlays" ];
  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot/efi";
      timeout = 1;
    };
    cleanTmpDir = true;
  };

  security.allowUserNamespaces = true;

  i18n.supportedLocales = [
    "en_GB.UTF-8/UTF-8"
    "de_DE.UTF-8/UTF-8"
  ];

  networking = {
    hostName = "beta32";
    enableIPv6 = false;
    dhcpcd.enable = false;
    useDHCP = false;
    interfaces.enp0s31f6.ipv4.addresses = [{
      address = "192.168.178.3";
      prefixLength = 24;
    }];
    defaultGateway.address = "192.168.178.1";
    nameservers = [ "192.168.178.1" ];
  };

  hardware.cpu.intel.updateMicrocode = true;

  services.openssh = {
    enable = true;
    permitRootLogin = "without-password";
    passwordAuthentication = false;
  };

  documentation.enable = pkgs.lib.mkForce false;

  system.stateVersion = "19.09";
}
