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
  libsecret,
}:
mkDerivation rec {
  pname = "chatterino2";
  version = "nightly";
  src = fetchFromGitHub {
    owner = "Chatterino";
    repo = pname;
    rev = "adf58d2770c8d6894bebcbb3c2435279493bd385";
    sha256 = "sha256-+CdHP+yrixVBiCBSvXHRqyfV5WhWig6Q1fHBeTqpu1Q=";
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
