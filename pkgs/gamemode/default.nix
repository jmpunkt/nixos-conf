{ stdenv, fetchFromGitHub, lib, meson, ninja, pkgconfig, cmake, systemd, dbus
, polkit, jmpunkt,

# provides the nvidia-settings package which is required to control
# Nvidia cards, otherwise the executable is compiled with a generic path
withNvidiaPackage ? null }:

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

  patches = [ ./polkit-policy.patch ];

  mesonFlags = [
    "-Dwith-systemd-user-unit-dir=${placeholder "out"}/etc/systemd/system"
    "-Dwith-sd-bus-provider=systemd"
    "-Dwith-examples=false"
    "-Dwith-pam-group=gamemode"
  ];

  preConfigure = ''
    for f in daemon/gamemode-gpu.c daemon/gamemode-context.c; do
        substituteInPlace $f \
            --replace '/usr/bin/pkexec' "${polkit}/bin/pkexec"
    done
  '' + stdenv.lib.optionalString (withNvidiaPackage != null) ''
    for f in util/gpuclockctl.c; do
        substituteInPlace $f \
            --replace '/usr/bin/nvidia-settings' "${withNvidiaPackage}/bin/nvidia-settings"
    done
  '';

  postInstall = ''
    substituteInPlace $out/bin/gamemoderun \
        --replace libgamemodeauto.so.0 $out/lib/libgamemodeauto.so.0:$out/lib/libgamemode.so.0:
  '';

  # So the polkit policy can reference /run/current-system/sw/bin/gamemode-daemon
  postFixup = ''
    mkdir -p $out/bin/gamemode-daemon
    ln -s $out/libexec/cpugovctl $out/bin/gamemode-daemon/cpugovctl
    ln -s $out/libexec/gpuclockctl $out/bin/gamemode-daemon/gpuclockctl
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
