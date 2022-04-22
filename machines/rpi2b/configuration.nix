{
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    <nixpkgs/nixos/modules/installer/sd-card/sd-image-armv7l-multiplatform.nix>
    ../../configurations/base.nix
    ../../configurations/users/jonas.nix
  ];

  nixpkgs.config.allowUnsupportedSystem = true;
  nixpkgs.crossSystem.system = "armv7l-linux";

  nix.binaryCaches = lib.mkForce ["https://app.cachix.org/cache/thefloweringash-armv7"];
  nix.binaryCachePublicKeys = ["thefloweringash-armv7.cachix.org-1:v+5yzBD2odFKeXbmC+OPWVqx4WVoIVO6UXgnSAWFtso="];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };
}
