{
  lib,
  stdenv,
  fetchFromGitHub,
  meson,
  wrapGAppsHook,
  gtk3,
  glib,
}:
stdenv.mkDerivation rec {
  pname = "oh-my-svg";
  version = "nightly";

  src = fetchFromGitHub {
    repo = "sonnyp";
    owner = "OhMySVG";
    rev = "3ad34251919beabc60d2aeae802e019b7bafcfdd";
    sha256 = "sha256-/tef881ZusYvJxVcm91p7vh1hwuXHm3LCqOMCT0TGXE=";
  };

  nativeBuildInputs = [
    meson
    wrapGAppsHook
  ];

  buildInputs = [
    glib
    gtk3
  ];

  meta = with lib; {
    description = "Oh My SVG let you export unoptimized SVG files into smaller versions.";
    homepage = "https://github.com/sonnyp/OhMySVG";
    license = licenses.gpl3;
    platforms = platforms.unix;
  };
}
