{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "d0e9666e7493ff5267f45646bcba737a73873c13";
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
      sha256 = "sha256-tEkraQvnoZQv7r0Pa5yhexNYAjwbK+tUnLjNkrXSbeA=";
    };

    recipe = writeText "recipe" ''
      (ox-typst :repo "jmpunkt/ox-typst" :fetcher github)
    '';

    meta = with lib; {
      description = "Org-mode to Typst exporter";
      license = licenses.gpl3;
    };
  }
