{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.minimal;
in
{
  options.profiles.minimal = {
    enable = lib.mkEnableOption "minimal system tools";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      curl
      wget
      fd
      git
      hdparm
      htop
      inetutils
      pciutils
      ripgrep
      sdparm
      smartmontools
      srm
      unzip
      usbutils

      # filesystem utils
      dosfstools
      f2fs-tools
      jfsutils
      ntfs3g
      ntfsprogs
      xfsprogs.bin
    ];
  };
}
