{
  systemConfig,
  lib,
  pkgs,
  ...
}:

let
  cfg = systemConfig.profiles.desktop.cosmic;
in
{
  config = lib.mkIf cfg.enable {
    qt = {
      enable = true;
      platformTheme.name = "qt5ct";
      # style.name = "Breeze";
    };
    services.gpg-agent = {
      pinentry.package = pkgs.pinentry-gnome3;
    };

    # NOTE: Required for pinentry-gnome3 to work
    dbus.packages = with pkgs; [ gcr ];
  };
}
