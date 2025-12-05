{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.virtualization;
in
{
  options.profiles.virtualization = {
    enable = lib.mkEnableOption "virtualization support";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.virtualbox.host.enable = true;
    boot.kernelParams = [ "kvm.enable_virt_at_load=0" ];
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
    virtualisation = {
      podman = {
        enable = true;
        dockerCompat = true;
      };
    };
  };
}
