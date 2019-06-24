{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
    gnupg
  ];

  services.pcscd.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  services.udev.packages = [ pkgs.yubikey-personalization ];

  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';
}
