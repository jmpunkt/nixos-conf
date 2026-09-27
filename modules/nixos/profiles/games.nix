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
        proton-run
        wineWow64Packages.full
        wineWow64Packages.fonts
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
