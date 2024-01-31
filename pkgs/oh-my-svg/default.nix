{
  lib,
  stdenv,
  fetchFromGitHub,
  meson,
  ninja,
  cmake,
  pkg-config,
  wrapGAppsHook4,
  gtk4,
  glib,
  gjs,
  appstream-glib,
  desktop-file-utils,
  gobject-introspection,
  libadwaita,
}:
stdenv.mkDerivation rec {
  pname = "oh-my-svg";
  version = "nightly";

  # dontPatchShebangs = true;

  src = fetchFromGitHub {
    owner = "sonnyp";
    repo = "OhMySVG";
    rev = "3ad34251919beabc60d2aeae802e019b7bafcfdd";
    sha256 = "sha256-6Tn1dr+K9XC7Nf2Wap0eFjOJtDmWfk/oDoWn4Cu6Yvo=";
  };

  nativeBuildInputs = [
    meson
    cmake
    ninja
    pkg-config
    wrapGAppsHook4
    desktop-file-utils
    gobject-introspection
    appstream-glib
  ];

  patchPhase = ''
    # sed -i 's;#!/usr/bin/env -S gjs -m;#!/usr/bin/env -S ${gjs}/bin/gjs -m;g' ./**/re.sonny.OhMySVG
    sed -i 's;#!/usr/bin/env -S gjs -m;#!/usr/bin/env gjs -m;g' ./**/re.sonny.OhMySVG
  '';

  buildInputs = [
    glib
    gtk4
    gjs
    libadwaita
  ];

  meta = with lib; {
    description = "Oh My SVG let you export unoptimized SVG files into smaller versions.";
    homepage = "https://github.com/sonnyp/OhMySVG";
    license = licenses.gpl3;
    platforms = platforms.unix;
    mainProgram = "re.sonny.OhMySVG";
  };
}
