{
  mkDerivation,
  stdenv,
  lib,
  pkg-config,
  fetchFromGitHub,
  qtbase,
  qtsvg,
  qtmultimedia,
  qtkeychain,
  cmake,
  boost,
  openssl,
  wrapQtAppsHook,
  qttools,
  qtimageformats,
}:
mkDerivation rec {
  pname = "chatterino2";
  version = "nightly";
  src = fetchFromGitHub {
    owner = "Chatterino";
    repo = pname;
    rev = "82797898c12e173e8300941eee04d4038fd01352";
    sha256 = "sha256-iVxR2dc+dYAEfDOLZvjyJv131JEPnnfqyML59vxKQVs=";
    fetchSubmodules = true;
  };
  nativeBuildInputs = [cmake pkg-config wrapQtAppsHook];
  buildInputs = [
    qtbase
    qtsvg
    qtmultimedia
    boost
    openssl
    qtkeychain
    qttools
    # NOTE: not required for building, but required for 7TV emotes
    #       (includes webp format)
    qtimageformats
  ];
  postInstall =
    lib.optionalString stdenv.isDarwin ''
      mkdir -p "$out/Applications"
      mv bin/chatterino.app "$out/Applications/"
    ''
    + ''
      mkdir -p $out/share/icons/hicolor/256x256/apps
      cp $src/resources/icon.png $out/share/icons/hicolor/256x256/apps/chatterino.png
    '';
  meta.mainProgram = "chatterino";
}
