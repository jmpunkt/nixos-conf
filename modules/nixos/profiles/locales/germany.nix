{
  config,
  lib,
  ...
}:

let
  cfg = config.profiles.locales.germany;
in
{
  options.profiles.locales.germany = {
    enable = lib.mkEnableOption "Germany locale support";
  };

  config = lib.mkIf cfg.enable {
    i18n = {
      # Has 24hrs, proper date format, uses A4, and is English.
      defaultLocale = "en_IE.UTF-8";
      extraLocaleSettings = {
        LC_TIME = "de_DE.UTF-8";
        LC_MONETARY = "de_DE.UTF-8";
        LC_IDENTIFICATION = "de_DE.UTF-8";
        LC_TELEPHONE = "de_DE.UTF-8";
      };
    };
    console = {
      keyMap = lib.mkDefault "de";
    };
    location = {
      latitude = 50.11;
      longitude = 8.682;
    };
    time.timeZone = "Europe/Berlin";
  };
}
