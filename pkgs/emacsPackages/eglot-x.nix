{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "5b5a56ddf8a087520f5e25a7343ec6503338e35b";
  pname = "eglot-x";
  owner = "nemethf";
in
  melpaBuild {
    inherit pname;
    version = "20250216.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev owner;
      repo = pname;
      sha256 = "sha256-6FY/Y5ahA82N0uJ0Dvb5e390UoVBEi8E8kKGfyknM/k=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "Protocol extensions for Eglot";
      license = licenses.gpl3;
    };
  }
