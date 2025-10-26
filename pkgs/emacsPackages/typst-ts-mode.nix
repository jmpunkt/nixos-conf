{
  lib,
  fetchFromGitea,
  melpaBuild,
  writeText,
}:
let
  rev = "7c2ef0d5bd2b5a8727fe6d00938c47ba562e0c94";
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
    sha256 = "sha256-D+QEfEYlxJICcdUCleWpe7+HxePLSSmV7zAwvyTL0+Q=";
  };

  recipe = writeText "recipe" ''
    (${pname} :repo "${owner}/emacs-${pname}" :fetcher codeberg)
  '';

  meta = with lib; {
    description = "Typst tree sitter major mode for Emacs. ";
    license = licenses.gpl3;
  };
}
