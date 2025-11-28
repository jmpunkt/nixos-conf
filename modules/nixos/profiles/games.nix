{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.games;
in
{
  options.profiles.games = {
    enable = lib.mkEnableOption "basic game support";
  };

  config = lib.mkIf cfg.enable (
    # assert config.profiles.desktop.enable;
    let
      proton-run =
        let
          inherit (pkgs)
            writeShellScriptBin
            unstable
            lib
            proton-ge-bin
            ;
          inherit (unstable)
            umu-launcher
            ;
        in
        writeShellScriptBin "proton-run" ''
          export GAMEID=umu-default2
          export PROTONPATH="${lib.makeSearchPathOutput "steamcompattool" "" [ proton-ge-bin ]}"
          exec ${lib.getBin umu-launcher}/bin/umu-run "$@"
        '';
    in
    {
      programs.steam.enable = true;
      environment.systemPackages = with pkgs; [
        bottles
        # (lutris.override {
        #   extraPkgs =
        #     pkgs: with pkgs; [
        #       wineWowPackages.unstableFull
        #       wineWowPackages.fonts
        #       winetricks
        #       dxvk
        #       samba
        #       vkd3d

        #       pixman
        #       libjpeg
        #       vulkan-loader
        #       vulkan-tools

        #       xorg.libXcursor
        #       xorg.libXi
        #       xorg.libXinerama
        #       xorg.libXScrnSaver
        #       libpng
        #       libpulseaudio
        #       libvorbis
        #       stdenv.cc.cc.lib
        #       libkrb5
        #       keyutils
        #     ];
        # })
        proton-run
        wineWowPackages.unstableFull
        wineWowPackages.fonts
        winetricks

        # wine dependencies
        dxvk
        samba
        vkd3d
        vulkan-loader
        vulkan-tools
        pixman
        libjpeg
      ];

      hardware.graphics.enable32Bit = true;
    }
  );
}
