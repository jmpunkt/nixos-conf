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
    ../../configurations/desktop/minimal.nix
  ];
  boot.supportedFilesystems.zfs = lib.mkForce false;
  isoImage.volumeID = "nixos-${config.system.nixos.release}-${pkgs.stdenv.hostPlatform.uname.processor}";
  environment.systemPackages = with pkgs; [
    gparted
    dosfstools
    sleuthkit
    libsForQt5.ark
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
  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
  };
  services.displayManager = {
    defaultSession = "xfce";
    autoLogin = {
      enable = true;
      user = "nixos";
    };
  };
}
