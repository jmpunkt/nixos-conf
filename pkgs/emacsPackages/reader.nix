{
  lib,
  fetchFromGitea,
  melpaBuild,
  gcc,
  unstable,
  gnumake,
  pkg-config,
}:
let
  rev = "e2d790b337029489c46443131b0e1c9c710a5eb6";
  ename = "reader";
  pname = "emacs-reader";
  owner = "divyaranjan";
in
melpaBuild {
  inherit pname ename;

  version = "20250702.0";

  commit = rev;

  src = fetchFromGitea {
    inherit rev owner;
    domain = "codeberg.org";
    repo = pname;
    sha256 = "sha256-O031rpG2kQo31QFKKw46Bjwy4EnWIqYuecs972ZzEDg=";
  };

  files = ''(:defaults "render-core.so")'';

  preBuild = ''
    make all
  '';

  nativeBuildInputs = [
    gcc
    gnumake
    pkg-config
  ];

  buildInputs = [
    unstable.mupdf
  ];

  meta = with lib; {
    description = "An all-in-one document reader for all formats in Emacs, backed by MuPDF.";
    license = licenses.gpl3;
  };
}
