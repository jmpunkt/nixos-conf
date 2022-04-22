{
  lib,
  stdenv,
  emacs,
  fetchFromGitHub,
  emacsTrivialBuild,
}:
emacsTrivialBuild
rec {
  pname = "eglot-x.el";
  version = "2022-04-01";
  src =
    fetchFromGitHub
    {
      owner = "nemethf";
      repo = pname;
      rev = "b56fbab9754030959f605cccb5d1f2cf7dd07616";
      sha256 = "cFa1fL7qy1ocjTsQdWxciojTKNTjc6jVUkdvIN2AiKg=";
    };
  meta = with lib; {
    description = "Protocol extensions for Eglot";
    license = licenses.gpl3;
  };
}
