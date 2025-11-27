{
  config,
  pkgs,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
    yubioath-flutter
  ];
  services = {
    pcscd.enable = true;
    udev.packages = [ pkgs.yubikey-personalization ];
  };
}
