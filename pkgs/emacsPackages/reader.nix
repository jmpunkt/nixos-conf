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
  rev = "6f0a11009435fbe62077a452f8b2bfeabb2806cc";
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
    sha256 = "sha256-AxYaH9HZo7iF1IZc5suJDrSmKSelDcJaOHwdaoBsb0k=";
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

  meta = {
    description = "An all-in-one document reader for all formats in Emacs, backed by MuPDF.";
    license = lib.licenses.gpl3;
  };
}
