{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "295c0309dc836966467c95867d1593f1376507b6";
  pname = "eglot-x";
  owner = "nemethf";
in
  melpaBuild {
    inherit pname;
    version = "20220401.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev owner;
      repo = pname;
      sha256 = "sha256-G/jnEQRVo6xpBaW5cBrcAD03P65stgGMhTM21pxdNvE=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "Protocol extensions for Eglot";
      license = licenses.gpl3;
    };
  }
