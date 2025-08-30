{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.steam.enable = true;
  environment.systemPackages = with pkgs; [
    bottles
    (lutris.override {
      extraPkgs =
        pkgs: with pkgs; [
          wineWowPackages.unstableFull
          wineWowPackages.fonts
          winetricks
          dxvk
          samba
          vkd3d

          pixman
          libjpeg
          vulkan-loader
          vulkan-tools

          xorg.libXcursor
          xorg.libXi
          xorg.libXinerama
          xorg.libXScrnSaver
          libpng
          libpulseaudio
          libvorbis
          stdenv.cc.cc.lib
          libkrb5
          keyutils
        ];
    })
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
