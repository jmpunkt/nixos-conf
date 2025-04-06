{
  lib,
  fetchFromGitea,
  melpaBuild,
  writeText,
}: let
  rev = "1367003e2ad55a2f6f9e43178584683028ab56e9";
  pname = "typst-ts-mode";
  owner = "meow_king";
in
  melpaBuild {
    inherit pname;
    version = "20241207.0";

    commit = rev;

    src = fetchFromGitea {
      inherit owner rev;
      domain = "codeberg.org";
      repo = pname;
      sha256 = "sha256-0RAJ/Td3G7FDvzf7t8csNs/uc07WUPGvMo8ako5iyl0=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/emacs-${pname}" :fetcher codeberg)
    '';

    meta = with lib; {
      description = "Typst tree sitter major mode for Emacs. ";
      license = licenses.gpl3;
    };
  }
