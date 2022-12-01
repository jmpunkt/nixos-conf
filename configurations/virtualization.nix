{
  config,
  pkgs,
  lib,
  ...
}: {
  virtualisation.libvirtd.enable = true;
  environment.systemPackages = with pkgs; [
    virt-manager-qt
  ];
  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.dnsname.enable = true;
    };
  };
}
