{
  lib,
  fetchFromGitea,
  melpaBuild,
  writeText,
}: let
  rev = "662e681087e6dddc5aae252d4efabb1fa1f83e81";
  pname = "xdg-appmenu";
  owner = "akib";
in
  melpaBuild {
    inherit pname;
    version = "20230901.0";

    commit = rev;

    src = fetchFromGitea {
      inherit owner rev;
      domain = "codeberg.org";
      repo = "emacs-xdg-appmenu";
      hash = "sha256-RCXyfcGQxSseq77bSbMsPk26gi5TkoIlYEM9kGNFADM=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/emacs-${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "Run XDG desktop applications";
    };
  }
