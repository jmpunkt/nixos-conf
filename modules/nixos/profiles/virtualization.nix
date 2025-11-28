{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.virtualisation;
in
{
  options.profiles.virtualisation = {
    enable = lib.mkEnableOption "virtualization support";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.virtualbox.host.enable = true;
    boot.kernelParams = [ "kvm.enable_virt_at_load=0" ];
    virtualisation.libvirtd.enable = true;
    environment.systemPackages = with pkgs; [
      virt-manager-qt
    ];
    virtualisation = {
      podman = {
        enable = true;
        dockerCompat = true;
      };
    };
  };
}
