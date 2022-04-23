{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  melpaBuild,
  writeText,
  eglot,
}: let
  rev = "b56fbab9754030959f605cccb5d1f2cf7dd07616";
  pname = "eglot-x";
in
  melpaBuild {
    inherit pname;
    version = "20220401.0";

    commit = rev;

    packageRequires = [eglot];

    src = fetchFromGitHub {
      inherit rev;
      owner = "nemethf";
      repo = pname;
      sha256 = "WHPV4VTq1/ahtJxRCN+qHHCYKE7YvJdyHNMesfhTM8Y=";
    };

    recipe = writeText "recipe" ''
      (eglot-x
      :repo "nemethf/eglot-x"
      :fetcher github)
    '';

    meta = with lib; {
      description = "Protocol extensions for Eglot";
      license = licenses.gpl3;
    };
  }
