{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.yubikey;
in
{
  options.profiles.yubikey = {
    enable = lib.mkEnableOption "Yubikey support";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      yubikey-personalization
    ];
    services = {
      pcscd.enable = true;
      udev.packages = [ pkgs.yubikey-personalization ];
    };
  };

}
