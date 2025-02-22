{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
  dash,
  editorconfig,
  s,
  f,
  jsonrpc,
}: let
  rev = "8a0068afcfb98af7f172d04f6f8cc932c1a22fe8";
  pname = "copilot";
  owner = "copilot-emacs";
in
  melpaBuild {
    inherit pname;
    version = "20250219.0";

    commit = rev;
    packageRequires = [
      dash
      editorconfig
      s
      f
      jsonrpc
    ];

    src = fetchFromGitHub {
      inherit rev owner;
      repo = "${pname}.el";
      sha256 = "sha256-7UIu/qVFI27lpwkX13k5CvuJbP9oted3KrT0vN/dJ1E=";
    };

    recipe = writeText "recipe" ''
      (copilot :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "An unofficial Copilot plugin for Emacs.";
    };
  }
