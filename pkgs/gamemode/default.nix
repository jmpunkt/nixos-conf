{ stdenv, fetchFromGitHub, lib, meson, ninja, pkgconfig, cmake, systemd, dbus
, jmpunkt }:

stdenv.mkDerivation rec {
  name = "gamemode";
  version = "1.6";

  src = fetchFromGitHub {
    owner = "FeralInteractive";
    repo = "gamemode";
    rev = version;
    sha256 = "195h6q8157cq1nqxs0zyl8y7awcqqq5snwrrgyphvrqzknp5mk09";
  };

  nativeBuildInputs = [ ninja meson pkgconfig cmake ];

  buildInputs = [ systemd dbus jmpunkt.inih ];

  mesonFlags = [
    "-Dwith-systemd-user-unit-dir=${placeholder "out"}/etc/systemd/system"
    "-Dwith-examples=false"
  ];

  postInstall = ''
    substituteInPlace $out/bin/gamemoderun \
        --replace libgamemodeauto.so.0 $out/lib/libgamemodeauto.so.0:$out/lib/libgamemode.so.0:
  '';

  meta = with lib; {
    description =
      "GameMode is a daemon/lib combo for Linux that allows games to request a set of optimisations be temporarily applied to the host OS and/or a game process.";
    homepage = "https://github.com/FeralInteractive/gamemode";
    license = with licenses; [ bsd3 ];
    maintainers = with maintainers; [ ];
    platforms = lib.platforms.linux;
  };
}
