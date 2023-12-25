{
  lib,
  fetchFromSourcehut,
  melpaBuild,
  writeText,
}: let
  rev = "eb4bcb0a85a0ad96841cfe2112f378d7ab673758";
  pname = "typst-ts-mode";
in
  melpaBuild {
    inherit pname;
    version = "20231225.0";

    commit = rev;

    src = fetchFromSourcehut {
      inherit rev;
      owner = "~meow_king";
      repo = pname;
      sha256 = "sha256-+0j1alT0cVfxXdWie6J2UWX1JNx9EX3seDpOWor0klA=";
    };

    recipe = writeText "recipe" ''
      (typst-ts-mode :repo "meow_king/typst-ts-mode" :fetcher sourcehut)
    '';

    meta = with lib; {
      description = "Typst tree sitter major mode for Emacs. ";
      license = licenses.gpl3;
    };
  }
