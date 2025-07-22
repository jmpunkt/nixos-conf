{
  lib,
  config,
  pkgs,
  ...
}:
{
  users.users.jonas = {
    isNormalUser = true;
    uid = 1000;
    extraGroups =
      [
        config.users.groups.wheel.name
        config.users.groups.audio.name
        config.users.groups.users.name
      ]
      ++ (lib.optionals config.virtualisation.libvirtd.enable [
        config.users.groups.libvirtd.name
      ])
      ++ (lib.optionals config.networking.networkmanager.enable [
        config.users.groups.networkmanager.name
      ]);
    createHome = true;
    home = "/home/jonas";
    shell = pkgs.fish;
    group = config.users.groups.jonas.name;
    openssh.authorizedKeys.keys = [
      (builtins.readFile ./../../home/jonas/yubikey/ssh.pub)
    ];
  };
  users.groups.jonas = {
    gid = 1000;
  };
}
