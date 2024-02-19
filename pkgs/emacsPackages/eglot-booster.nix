{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "e79dea640356eb4a8ed9df3808fe73c7c6db4cc4";
  pname = "eglot-booster";
in
  melpaBuild {
    inherit pname;
    version = "20220401.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev;
      owner = "jdtsmith";
      repo = pname;
      sha256 = "sha256-ybNqMHCGjzT2+4OfywS7hNw551kIzwI3QqC8tU/GsQI=";
    };

    recipe = writeText "recipe" ''
      (eglot-booster :repo "jdtsmith/eglot-booster" :fetcher github)
    '';

    meta = with lib; {
      description = "Boost eglot using lsp-booster";
      license = licenses.gpl3;
    };
  }
