{ config, pkgs, ... }:

{
  imports = [ ../yubico.nix ];

  users = {
    groups.jonas.gid = 1000;
    users.jonas = {
      extraGroups = ["wheel" "networkmanager" "audio" "users"];
      createHome = true;
      home = "/home/jonas";
      shell = pkgs.fish;
      uid = 1000;
      group = "jonas";
    };
  };
}
