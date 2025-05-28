{
  config,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    yubikey-personalization
    yubikey-personalization-gui
  ];
  services = {
    pcscd.enable = true;
    udev.packages = [pkgs.yubikey-personalization];
  };
}
