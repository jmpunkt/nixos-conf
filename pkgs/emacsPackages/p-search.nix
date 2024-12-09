{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
  heap,
}: let
  rev = "31e88b24985b6bd5c293a29bb135c480e04d86f8";
  pname = "p-search";
  owner = "zkry";
in
  melpaBuild {
    inherit pname;
    version = "20241209.0";

    commit = rev;
    packageRequires = [
      heap
    ];

    src = fetchFromGitHub {
      inherit rev owner;
      repo = pname;
      sha256 = "sha256-URWOMJfzCW4oabKHkaW2bH0UpaNZY/3ohcZnaPQ4uS4=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "Protocol extensions for Eglot";
      license = licenses.gpl3;
    };
  }
