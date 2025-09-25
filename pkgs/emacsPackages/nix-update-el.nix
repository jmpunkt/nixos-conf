{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}:
let
  rev = "d67f4f7ba8c8ec43144600f5f970c5fd958fc2f7";
  pname = "nix-update";
  owner = "jwiegley";
in
melpaBuild {
  inherit pname;
  version = "20250817.0";

  commit = rev;

  src = fetchFromGitHub {
    inherit owner rev;
    repo = "${pname}-el";
    sha256 = "sha256-IF5kuZsYb17yOt8Ijsz2sq7fOB+4siPG7WKb8Jx0NZQ=";
  };

  recipe = writeText "recipe" ''
    (${pname} :repo "${owner}/${pname}-el" :fetcher github)
  '';

  meta = with lib; {
    description = "An Emacs command for updating fetch declarations in place";
  };
}
