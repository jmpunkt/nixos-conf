{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "fef96db4b3d8795d306375538bcbd87a8453dd3e";
  pname = "consult";
  owner = "jmpunkt";
in
  melpaBuild {
    inherit pname;
    version = "20250304.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev owner;
      repo = "${pname}";
      sha256 = "sha256-CSSe+G9x9cyRynOuaHh3PQnV6fvEYf/55pTXkl88l8E=";
    };

    recipe = writeText "recipe" ''
      (consult :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "consult";
    };
  }
