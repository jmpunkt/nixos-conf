{ config, pkgs, ... }:

{
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.cpu.intel.updateMicrocode = true;
}
