{
  lib,
  fetchFromGitea,
  melpaBuild,
  writeText,
}: let
  rev = "d3e44b5361ed1bbb720a38dafdb29cb8d8b6d8be";
  pname = "typst-ts-mode";
  owner = "meow_king";
in
  melpaBuild {
    inherit pname;
    version = "20240317.0";

    commit = rev;

    src = fetchFromGitea {
      inherit owner rev;
      domain = "codeberg.org";
      repo = pname;
      sha256 = "sha256-fECXfTjbckgS+kEJ3dMQ7zDotqdxxBt3WFl0sEM60Aw=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/emacs-${pname}" :fetcher codeberg)
    '';

    meta = with lib; {
      description = "Typst tree sitter major mode for Emacs. ";
      license = licenses.gpl3;
    };
  }
