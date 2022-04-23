{
  stdenv,
  lib,
  fetchgit,
  cmake,
  extra-cmake-modules,
  kdoctools,
  kconfig,
  kio,
  wrapQtAppsHook,
  qtquickcontrols2,
  mpv,
  ffmpeg,
  youtube-dl,
  breeze-qt5,
  kfilemetadata,
}:
stdenv.mkDerivation
rec {
  pname = "haruna";
  version = "0.7.2";
  src =
    fetchgit
    {
      url = "https://invent.kde.org/multimedia/haruna";
      rev = "v${version}";
      sha256 = "0s4v3YJhSssp2S9mppMXq0AtWXPIaqOYWPmJgKjXjDE=";
    };
  nativeBuildInputs = [cmake extra-cmake-modules kdoctools wrapQtAppsHook];
  buildInputs = [mpv ffmpeg youtube-dl kconfig kfilemetadata qtquickcontrols2 kio breeze-qt5];
  meta = with lib; {
    homepage = "https://invent.kde.org/multimedia/haruna";
    license = with licenses; [gpl2];
    maintainers = with maintainers; [];
    platforms = platforms.linux;
  };
}
