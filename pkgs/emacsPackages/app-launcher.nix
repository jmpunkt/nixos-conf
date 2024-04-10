{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "d5015e394b0a666a8c7c4d4bdf786266e773b145";
  pname = "app-launcher";
  owner = "SebastienWae";
in
  melpaBuild {
    inherit pname;
    version = "20220416.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev owner;
      repo = pname;
      sha256 = "sha256-d0d5rkuxK/zKpSCa1UTdpV7o+RDDsEeab56rI7xUJ1E=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "Launch application from Emacs ";
    };
  }
