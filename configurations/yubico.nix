{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
    gnupg
  ];

  services = {
    pcscd.enable = true;
    udev.packages = [ pkgs.yubikey-personalization ];
  };

  programs = {
    gnupg.agent = { enable = true; enableSSHSupport = true; };
    ssh.startAgent = false;
  };
}
