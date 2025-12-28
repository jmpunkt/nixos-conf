{
  lib,
  fetchFromGitHub,
  melpaBuild,
  writeText,
}:
let
  rev = "b92c44e6b34f8df0539d3c8ab5992c5a7eb815d5";
  pname = "eglot-x";
  owner = "nemethf";
in
melpaBuild {
  inherit pname;
  version = "20251219.0";

  commit = rev;

  src = fetchFromGitHub {
    inherit rev owner;
    repo = pname;
    sha256 = "sha256-VvamDqZ3NowM6XfRlC2exsM6ssRBqWUw6ziKgqdphwM=";
  };

  recipe = writeText "recipe" ''
    (${pname} :repo "${owner}/${pname}" :fetcher github)
  '';

  meta = with lib; {
    description = "Protocol extensions for Eglot";
    license = licenses.gpl3;
  };
}
