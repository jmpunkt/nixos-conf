{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "535ef61e82f09d744cd5b097b1fc99f08cce175c";
  pname = "copilot";
  owner = "copilot-emacs";
in
  melpaBuild {
    inherit pname;
    version = "20240313.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev owner;
      repo = "${pname}.el";
      sha256 = "sha256-/ZDnEZWUFcKnUtFrd/4C7LX16GAdUQncU8ZnYzntKS0=";
    };

    recipe = writeText "recipe" ''
      (copilot :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "An unofficial Copilot plugin for Emacs.";
    };
  }
