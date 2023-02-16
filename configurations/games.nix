{
  config,
  pkgs,
  lib,
  ...
}: let
  wine = pkgs.wineWowPackages.staging;
  battle-net = let
    wine = pkgs.wineWowPackages.full.override {
      wineRelease = "staging";
      mingwSupport = true;
      tlsSupport = true;
      pulseaudioSupport = true;
      vulkanSupport = true;
      waylandSupport = true;
    };
  in
    pkgs.writeScriptBin "battle-net" ''
      export WINEARCH=win64
      export WINEPREFIX=$HOME/.wine-battlenet
      export PATH="${wine}/bin:${pkgs.winetricks}/bin:$PATH"

      battleNet="$WINEPREFIX/drive_c/Program Files (x86)/Battle.net/Battle.net.exe"

      if [ -f "$battleNet" ]; then
        wine64 "$battleNet" "$@"
      else
        winetricks dxvk
        wine64 "$@"
      fi
    '';
in {
  programs.firejail = {
    wrappedBinaries = {
      wine = {
        executable = "${lib.getBin wine}/bin/wine";
        profile = "${pkgs.firejail}/etc/firejail/wine.profile";
      };
      # lutris = {
      #   executable = "${lib.getBin pkgs.lutris}/bin/lutris";
      #   profile = "${pkgs.firejail}/etc/firejail/lutris.profile";
      # };
    };
  };
  programs.steam.enable = true;
  environment.systemPackages = with pkgs; [
    minecraft
    lutris
    samba
    battle-net
  ];
  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };
}
