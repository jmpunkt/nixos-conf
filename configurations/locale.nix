{config, ...}: {
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
    font = "Lat2-Terminus16";
    keyMap = "de";
  };
  time.timeZone = "Europe/Berlin";
}
