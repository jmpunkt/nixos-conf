{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "7d1724adf444bb0ddcf0c191c61365b1903031f0";
  pname = "eglot-x";
in
  melpaBuild {
    inherit pname;
    version = "20220401.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev;
      owner = "nemethf";
      repo = pname;
      sha256 = "sha256-MACRQnLH3bnZdtdDBsziIf6+IrArf0hykRLgwHhSiSE=";
    };

    recipe = writeText "recipe" ''
      (eglot-x :repo "nemethf/eglot-x" :fetcher github)
    '';

    meta = with lib; {
      description = "Protocol extensions for Eglot";
      license = licenses.gpl3;
    };
  }
