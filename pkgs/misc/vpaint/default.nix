{
  stdenv,
  lib,
  fetchFromGitHub,
  gcc,
  cmake,
  qtbase,
  wrapQtAppsHook,
  libGLU,
}:
stdenv.mkDerivation
rec {
  name = "vpaint";
  version = "unstable";
  src =
    fetchFromGitHub
    {
      owner = "dalboris";
      repo = "vpaint";
      rev = "6b1bf57e3c239194443f7284adfd5c5326cd1bf2";
      sha256 = "0lI+BeynkZ2RfTJmJ9pOVNQ+UcrRSi4r+6hALJ1SMss=";
    };
  nativeBuildInputs = [cmake qtbase wrapQtAppsHook];
  installPhase = ''
    mkdir -p $out/bin
    cp ./src/Gui/VPaint $out/bin/vpaint
  '';
  buildInputs = [libGLU];
}
