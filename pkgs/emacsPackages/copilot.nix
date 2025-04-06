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
  rev = "bb517382be5d0dc673f9381e9c2a0956dfc9dc45";
  pname = "copilot";
  owner = "copilot-emacs";
in
  melpaBuild {
    inherit pname;
    version = "20250404.0";

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
      sha256 = "sha256-OKjaeZaixOe0t2eZOoKQkhvH6eTN020lR1UxkGnH3pU=";
    };

    recipe = writeText "recipe" ''
      (copilot :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "An unofficial Copilot plugin for Emacs.";
    };
  }
