{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "08cbd4369618e60576c95c194e63403f080328ba";
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
      sha256 = "sha256-cWicqHYR/XU+71a8OFgF8vc6dmT/Fy0EEgzX0xvYiDc=";
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
