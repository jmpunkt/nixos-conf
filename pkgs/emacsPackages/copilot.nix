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
  rev = "88b10203705a9cdcbc232e7d2914f6b12217a885";
  pname = "copilot";
  owner = "copilot-emacs";
in
  melpaBuild {
    inherit pname;
    version = "20240313.0";

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
      sha256 = "sha256-oTAxayxrEiIu0GUtsqaL/pCY0ElU1RjZp7OXgqqJqnA=";
    };

    recipe = writeText "recipe" ''
      (copilot :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "An unofficial Copilot plugin for Emacs.";
    };
  }
