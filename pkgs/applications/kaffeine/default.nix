{ stdenv, fetchgit, extra-cmake-modules, kdoctools, gettext, isocodes, libX11
, libXScrnSaver, vlc, sqlite, xorg, qt5, libsForQt5 }:

stdenv.mkDerivation rec {
  pname = "kaffein";
  version = "2.0.18";

  src = fetchgit {
    url = "https://invent.kde.org/multimedia/kaffeine";
    rev = "v${version}";
    sha256 = "1rxavbv5agyayvzbfc9p10sw5cfd4g638hf3dqjpi41k17v95rhl";
  };

  nativeBuildInputs = [ extra-cmake-modules kdoctools gettext ];

  buildInputs = [
    vlc
    isocodes
    libXScrnSaver
    sqlite

    qt5.qtbase
    qt5.qtx11extras

    libsForQt5.ki18n
    libsForQt5.solid
    libsForQt5.kio
    libsForQt5.kdbusaddons
  ];

  meta = with stdenv.lib; {
    homepage = "https://invent.kde.org/multimedia/kaffeine";
    license = with licenses; [ gpl2 ];
    maintainers = with maintainers; [ ];
    platforms = stdenv.lib.platforms.linux;
  };
}
