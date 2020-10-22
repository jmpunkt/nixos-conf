{ stdenv, fetchurl, makeDesktopItem, makeWrapper, freetype, fontconfig, libX11
, libXrender, zlib, glib, gtk3, gtk2, libXtst, jdk, jdk8
, gsettings-desktop-schemas, webkitgtk ? null # for internal web browser
, buildEnv, runCommand, callPackage }:

let
  version = "1.0.0";
  gtk = gtk2;
  buildEclipse = callPackage ./build-eclipse.nix {
    inherit stdenv makeDesktopItem freetype fontconfig libX11 libXrender zlib
      jdk glib gtk libXtst gsettings-desktop-schemas webkitgtk makeWrapper;
  };
in buildEclipse {
  name = "papyrus-rt-${version}";
  description = "Eclipse IDE for Real Time";
  src = fetchurl {
    url =
      "https://www.eclipse.org/downloads/download.php?r=1&nf=1&file=/papyrus-rt/rcp/releases/oxygen/${version}/org.eclipse.papyrusrt.rcp.product-linux.gtk.x86_64.tar.gz";
    sha512 =
      "1y3g95261p78vj6g5z828sarjijplgg834mvp3paly3x6x7j411brp55ddylzgklm1avnnwn5iaxhyqnayvz2fj1xn6sgf2gf8r3i3s";
  };
}
