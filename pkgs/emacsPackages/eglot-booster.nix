{
  lib,
  fetchFromGitHub,
  melpaBuild,
}:
let
  rev = "cab7803c4f0adc7fff9da6680f90110674bb7a22";
  ename = "eglot-booster";
  pname = "eglot-booster";
  owner = "jdtsmith";
in
melpaBuild {
  inherit pname ename;

  version = "20250616.0";

  commit = rev;

  src = fetchFromGitHub {
    inherit rev owner;
    repo = pname;
    sha256 = "sha256-xUBQrQpw+JZxcqT1fy/8C2tjKwa7sLFHXamBm45Fa4Y=";
  };

  meta = {
    description = "Boost eglot using lsp-booster.";
    license = lib.licenses.gpl3;
  };
}
