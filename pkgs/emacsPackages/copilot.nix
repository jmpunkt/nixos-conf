{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "0d48e75dabef86b962b7a8bfcc7d3614a1201821";
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
      sha256 = "sha256-RYEq5bUouAZOGvsljM1d4tpMlFLmLC4VF7e7J/HrpKk=";
    };

    recipe = writeText "recipe" ''
      (copilot :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "An unofficial Copilot plugin for Emacs.";
    };
  }
