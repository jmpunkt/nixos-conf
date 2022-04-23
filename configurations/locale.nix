{config, ...}: {
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
    };
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "de";
  };
  services.xserver.layout = "de";
  time.timeZone = "Europe/Berlin";
}
