{ ... }:
{
  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "jonas" ];
  boot.kernelParams = [ "kvm.enable_virt_at_load=0" ];
}
