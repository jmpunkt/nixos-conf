{ stdenv, fetchFromGitHub, lib, meson, ninja, pkgconfig, cmake, systemd, dbus
, polkit, jmpunkt }:

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

  buildInputs = [ systemd dbus jmpunkt.inih polkit ];

  mesonFlags = [
    "-Dwith-systemd-user-unit-dir=${placeholder "out"}/lib/systemd/user"
    "-Dwith-sd-bus-provider=systemd"
    "-Dwith-examples=false"
  ];

  preConfigure = ''
    for f in daemon/gamemode-gpu.c daemon/gamemode-context.c; do
        substituteInPlace $f \
            --replace '/usr/bin/pkexec' "/run/current-system/sw/bin/pkexec"
    done

    for f in util/gpuclockctl.c; do
        substituteInPlace $f \
            --replace '/usr/bin/nvidia-settings' "/run/current-system/sw/bin/nvidia-settings"
    done
  '';

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
