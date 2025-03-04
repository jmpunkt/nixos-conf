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
  rev = "80fd2a503dd7a0e97ce9604e39bc62fa62441781";
  pname = "copilot";
  owner = "jmpunkt";
in
  melpaBuild {
    inherit pname;
    version = "20250304.0";

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
      sha256 = "sha256-NqXtdUdDxxK8xOslzBEXsB/nIlBInQmrTFZ7UPom7to=";
    };

    recipe = writeText "recipe" ''
      (copilot :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "An unofficial Copilot plugin for Emacs.";
    };
  }
