{ lib, stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "ligature.el";
  version = "2020-11-28";

  src = fetchFromGitHub {
    owner = "mickeynp";
    repo = name;
    rev = "c830b9d74dcf4ff08e6f19cc631d924ce47e2600";
    sha256 = "cFaXfL7qy1ocjTsQdWxciojTKNTjc6jVUkdvIN2AiKg=";
  };

  phases = [ "installPhase" ];

  installPhase = ''
    install -d "$out/share/emacs/site-lisp"
    install ${src}/ligature.el "$out/share/emacs/site-lisp"
  '';

  meta = with lib; {
    description = "Typographic Ligatures in Emacs";
    license = licenses.gpl3;
  };
}
