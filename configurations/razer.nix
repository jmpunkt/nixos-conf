{ config, pkgs, ... }:

{
  users.users.jonas = {
    extraGroups = [
      config.users.groups.plugdev.name
    ];
  };

  hardware.openrazer = {
    enable = true;
    syncEffectsEnabled = false;
    mouseBatteryNotifier = false;
  };

  environment.systemPackages = with pkgs; [
    razergenie
  ];
}
