{
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
  qt5compat,
  qtimageformats,
  libsecret,
}:
stdenv.mkDerivation rec {
  pname = "chatterino2";
  version = "nightly";
  src = fetchFromGitHub {
    owner = "Nerixyz";
    repo = pname;
    rev = "e3e14781f379388ba7a831634c51ea7f1f7f7aa5";
    sha256 = "sha256-I5rD28CvZGpjTKotq2D3djKiSUIbYC9YKXYkaM3yd90=";
    fetchSubmodules = true;
  };
  nativeBuildInputs = [cmake pkg-config wrapQtAppsHook];
  cmakeFlags = ["-DBUILD_WITH_QT6=on"];
  buildInputs = [
    qtbase
    qtsvg
    qt5compat
    qtmultimedia
    boost
    openssl
    qtkeychain
    qttools
    # NOTE: not required for building, but required for 7TV emotes
    #       (includes webp format)
    qtimageformats
    libsecret
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
