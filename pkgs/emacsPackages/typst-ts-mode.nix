{
  lib,
  fetchFromGitea,
  melpaBuild,
  writeText,
}:
let
  rev = "972dc69d6b8a3f8983f6b8000654f59c8a8d05ba";
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
    sha256 = "sha256-+x8AthOxO0vBjaKSaiDpLnf+Ph0AqWl7EMei29dr85g=";
  };

  recipe = writeText "recipe" ''
    (${pname} :repo "${owner}/emacs-${pname}" :fetcher codeberg)
  '';

  meta = with lib; {
    description = "Typst tree sitter major mode for Emacs. ";
    license = licenses.gpl3;
  };
}
