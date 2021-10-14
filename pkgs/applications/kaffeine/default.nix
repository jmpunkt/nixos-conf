{ stdenv
, lib
, fetchgit
, cmake
, wrapQtAppsHook
, extra-cmake-modules
, kdoctools
, gettext
, isocodes
, libX11
, libXScrnSaver
, vlc
, sqlite
, xorg
, qtbase
, qtx11extras
, ki18n
, solid
, kio
, kdbusaddons
}:

stdenv.mkDerivation rec {
  pname = "kaffeine";
  version = "2.0.18";

  src = fetchgit {
    url = "https://invent.kde.org/multimedia/kaffeine";
    rev = "v${version}";
    sha256 = "1rxavbv5agyayvzbfc9p10sw5cfd4g638hf3dqjpi41k17v95rhl";
  };

  nativeBuildInputs = [
    cmake
    extra-cmake-modules
    kdoctools
    gettext
    wrapQtAppsHook
  ];

  buildInputs = [
    vlc
    isocodes
    libXScrnSaver
    sqlite

    qtbase
    qtx11extras

    ki18n
    solid
    kio
    kdbusaddons
  ];

  meta = with lib; {
    homepage = "https://invent.kde.org/multimedia/kaffeine";
    license = with licenses; [ gpl2 ];
    maintainers = with maintainers; [];
    platforms = platforms.linux;
  };
}
