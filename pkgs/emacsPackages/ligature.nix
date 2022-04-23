{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  emacsTrivialBuild,
}:
emacsTrivialBuild
rec {
  pname = "ligature.el";
  version = "2020-11-28";
  src =
    fetchFromGitHub
    {
      owner = "mickeynp";
      repo = pname;
      rev = "c830b9d74dcf4ff08e6f19cc631d924ce47e2600";
      sha256 = "cFaXfL7qy1ocjTsQdWxciojTKNTjc6jVUkdvIN2AiKg=";
    };
  meta = with lib; {
    description = "Typographic Ligatures in Emacs";
    license = licenses.gpl3;
  };
}
