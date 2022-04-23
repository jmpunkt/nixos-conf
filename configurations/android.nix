{
  config,
  pkgs,
  ...
}: {
  users.users.jonas.extraGroups = ["adbusers"];
  programs.adb.enable = true;
  services.udev.packages = with pkgs; [android-udev-rules android-studio];
}
