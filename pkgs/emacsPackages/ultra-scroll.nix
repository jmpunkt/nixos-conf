{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}: let
  rev = "e89d15e18549bc6b3c41f21c3cf5082b04fea303";
  pname = "ultra-scroll";
  owner = "jdtsmith";
in
  melpaBuild {
    inherit pname;
    version = "20250109.0";

    commit = rev;

    src = fetchFromGitHub {
      inherit rev owner;
      repo = pname;
      sha256 = "sha256-XRaBUu1kJbuqkyhiyko22c+ZORCLu5zvAazrVeM51eY=";
    };

    recipe = writeText "recipe" ''
      (${pname} :repo "${owner}/${pname}" :fetcher github)
    '';

    meta = with lib; {
      description = "scroll emacs-mac like lightning";
      license = licenses.gpl3;
    };
  }
