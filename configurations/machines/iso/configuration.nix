{
  modulesPath,
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-base.nix"
  ];
  profiles = {
    desktop.xfce.enable = true;
    desktop.virtual = true;
    development.enable = true;
    locales.germany.enable = true;
  };
  boot.supportedFilesystems.zfs = lib.mkForce false;
  isoImage.volumeID = "nixos-${config.system.nixos.release}-${pkgs.stdenv.hostPlatform.uname.processor}";
  environment.systemPackages = with pkgs; [
    gparted
    dosfstools
    sleuthkit
    kdePackages.ark
    testdisk-qt
    wireshark
  ];
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("wheel")) {
        return polkit.Result.YES;
      }
    });
  '';
  isoImage = {
    squashfsCompression = "zstd -Xcompression-level 22 -b 32768";
  };
  powerManagement.enable = true;
  services.displayManager.autoLogin = {
    enable = true;
    user = "nixos";
  };
}
