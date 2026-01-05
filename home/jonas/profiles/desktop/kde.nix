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
    qt = {
      enable = true;
      platformTheme.name = "kde";
      style.name = "Breeze";
    };
    services.gpg-agent = {
      pinentry.package = pkgs.pinentry-qt;
    };

    programs.fuzzel.enable = true;
    stylix.targets.fuzzel.enable = true;
  };
}
