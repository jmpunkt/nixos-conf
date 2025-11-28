{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.development.android;
in
{
  options.profiles.development.android = {
    enable = lib.mkEnableOption "Android development tools";
  };

  config = lib.mkIf cfg.enable {
    users.users.jonas.extraGroups = [ "adbusers" ];
    programs.adb.enable = true;
    services.udev.packages = with pkgs; [
      android-udev-rules
      android-studio
    ];
  };
}
