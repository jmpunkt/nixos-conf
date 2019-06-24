{ config, pkgs, ... }:

{
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
    };
  };

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
    hunspellDicts.en-gb-ize
    hunspellDicts.en-us
    hunspellDicts.de-de

    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
  ];
}
