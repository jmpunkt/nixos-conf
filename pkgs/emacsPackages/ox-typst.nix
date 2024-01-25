{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "25ba84f38cd3ba95dc062feff05be3520d82c0ea";
  pname = "ox-typst";
in
  melpaBuild {
    inherit pname;
    version = "20240125.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev;
      owner = "jmpunkt";
      repo = pname;
      sha256 = "sha256-gI2TgMqOS8sVaaCKfKC5z6dCzgR0PSUiB5e3tDIJeKY=";
    };

    recipe = writeText "recipe" ''
      (ox-typst :repo "jmpunkt/ox-typst" :fetcher github)
    '';

    meta = with lib; {
      description = "Org-mode to Typst exporter";
      license = licenses.gpl3;
    };
  }
