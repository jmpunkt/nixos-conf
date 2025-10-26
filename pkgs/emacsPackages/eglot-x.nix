{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}:
let
  rev = "8e872efd3d0b7779bde5b1e1d75c8e646a1f729f";
  pname = "eglot-x";
  owner = "nemethf";
in
melpaBuild {
  inherit pname;
  version = "20250216.0";

  commit = rev;

  src = fetchFromGitHub {
    inherit rev owner;
    repo = pname;
    sha256 = "sha256-a2qkitikqGZBXI4aVdn8c7P4HFwep9RPWkOVBbgQV2g=";
  };

  recipe = writeText "recipe" ''
    (${pname} :repo "${owner}/${pname}" :fetcher github)
  '';

  meta = with lib; {
    description = "Protocol extensions for Eglot";
    license = licenses.gpl3;
  };
}
