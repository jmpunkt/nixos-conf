{
  config,
  pkgs,
  lib,
  ...
}: let
  # NOTE: do not enable wayland (currently it detects two monitors as
  # one and launches all battle-net related with twice the width by
  # same height.)
  wine = pkgs.wineWowPackages.staging;
  battle-net = pkgs.writeScriptBin "battle-net" ''
    export WINEARCH=win64
    export WINEPREFIX=$HOME/.wine-battlenet
    export PATH="${pkgs.wineWowPackages.staging}/bin:${pkgs.winetricks}/bin:$PATH"

    battleNet="$WINEPREFIX/drive_c/Program Files (x86)/Battle.net/Battle.net.exe"

    if [[ $# -eq 0 ]]; then
      if [ -f "$battleNet" ]; then
        # HACK: fixes failed starting battle-net because of missing QT
        find "$WINEPREFIX/drive_c/Program Files (x86)/Battle.net" -name "qwindows.dll" -exec setfattr -x user.DOSATTRIB "{}" \;
        wine "$battleNet" "$@"
      else
        echo "Battl-net not installed. Run battle-net -i <PATH_TO_BATTLENT_INSTALLER>"
        exit 1
      fi
    else
      case "$1" in
        install)
          winetricks dxvk
          wine "''${@:2}"
        ;;
        run)
          wine "''${@:2}"
        ;;
        winetricks)
          winetricks "''${@:2}"
        ;;
        *)
          echo "only install or run allowed."
          exit 1
        ;;
      esac
    fi

  '';
in {
  programs.firejail = {
    wrappedBinaries = {
      wine = {
        executable = "${lib.getBin wine}/bin/wine";
        profile = "${pkgs.firejail}/etc/firejail/wine.profile";
      };
    };
  };
  programs.steam.enable = true;
  environment.systemPackages = with pkgs; [
    minecraft
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
