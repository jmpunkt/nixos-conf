{
  lib,
  fetchFromSourcehut,
  melpaBuild,
  writeText,
}: let
  rev = "a5f094dd1d1ce992e9ad290e4838779b2dd5809a";
  pname = "typst-ts-mode";
in
  melpaBuild {
    inherit pname;
    version = "20240317.0";

    commit = rev;

    src = fetchFromSourcehut {
      inherit rev;
      owner = "~meow_king";
      repo = pname;
      sha256 = "sha256-HUOsb5aUj2Kb5E0HaZENp9pqQIAOF4t2SEIKH7cFspo=";
    };

    recipe = writeText "recipe" ''
      (typst-ts-mode :repo "meow_king/typst-ts-mode" :fetcher sourcehut)
    '';

    meta = with lib; {
      description = "Typst tree sitter major mode for Emacs. ";
      license = licenses.gpl3;
    };
  }
