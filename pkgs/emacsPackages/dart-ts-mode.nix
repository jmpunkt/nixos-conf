{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "61ec5ec504630a8142b5a647600dbfb31024fc38";
  pname = "dart-ts-mode";
in
  melpaBuild {
    inherit pname;
    version = "20231019.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev;
      owner = "50ways2sayhard";
      repo = pname;
      sha256 = "sha256-8DzmIQdpeyVPkEXFRebVKJPMIA3252ouL1xb4DlYdv4=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "50ways2sayhard/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "A major mode for editing Dart files with the power of treesit, provides syntax highlighting, indentation, and some emacs’ specific features like sexp, navigation and imenu.";
      license = licenses.gpl3;
    };
  }
