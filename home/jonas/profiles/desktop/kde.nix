{
  systemConfig,
  lib,
  pkgs,
  ...
}:

let
  cfg = systemConfig.profiles.desktop.kde;
in
{
  config = lib.mkIf cfg.enable {
    # gtk = {
    #   enable = true;
    #   font.name = "Noto Sans";
    #   iconTheme = {
    #     package = pkgs.kdePackages.breeze-icons;
    #     name = "breeze";
    #   };
    #   theme = {
    #     package = pkgs.kdePackages.breeze-gtk;
    #     name = "Breeze";
    #   };
    # };
    qt = {
      enable = true;
      platformTheme.name = "kde";
      style.name = "Breeze";
    };
    services.gpg-agent = {
      pinentry.package = pkgs.pinentry-qt;
    };
  };
}
