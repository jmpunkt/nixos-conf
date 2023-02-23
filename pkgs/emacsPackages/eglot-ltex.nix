{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "dbf45608aaa9bb61d540ce64f8b081cfa9876dd4";
  pname = "eglot-ltex";
in
  melpaBuild {
    inherit pname;
    version = "20221114.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev;
      owner = "emacs-languagetool";
      repo = pname;
      sha256 = "sha256-w/2DF3NRDrhZf90gJCSoY4vkAq4qmmqMQbdv2cr+CFg=";
    };

    recipe = writeText "recipe" ''
      (eglot-ltex
      :repo "emacs-languagetool/eglot-ltex"
      :fetcher github)
    '';

    meta = with lib; {
      description = "Ltex language server for eglot.";
      license = licenses.gpl3;
    };
  }
