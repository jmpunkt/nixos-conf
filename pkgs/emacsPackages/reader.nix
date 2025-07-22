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
  rev = "756a0e9c242dc4c4388cfd9323cb2b890ae9293e";
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
    sha256 = "sha256-HApjtyuFvQH5mIcqZxNRSSaDG7aAHS1o9Up9piUaCKo=";
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
