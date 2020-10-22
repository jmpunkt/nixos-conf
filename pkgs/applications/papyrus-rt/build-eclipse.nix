# Based on https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/eclipse/build-eclipse.nix (2355d9d545fe2b6ee8a603913d7ec3f6bafe9bef)
{ stdenv, makeDesktopItem, freetype, fontconfig, libX11, libXrender, zlib, jdk
, glib, gtk, libXtst, gsettings-desktop-schemas, webkitgtk, makeWrapper, ... }:

{ name, src ? builtins.getAttr stdenv.hostPlatform.system sources
, sources ? null, description }:

let
  name = "Papyrus-RT";
  exec = "papyrusrt";
in stdenv.mkDerivation rec {
  inherit name src;

  desktopItem = makeDesktopItem {
    name = name;
    exec = exec;
    icon = exec;
    comment = "Integrated Development Environment for Real Time";
    desktopName = "PapyrusRT IDE";
    genericName = "Integrated Development Environment";
    categories = "Application;Development;";
  };

  buildInputs = [
    fontconfig
    freetype
    glib
    gsettings-desktop-schemas
    gtk
    jdk
    libX11
    libXrender
    libXtst
    makeWrapper
    zlib
  ] ++ stdenv.lib.optional (webkitgtk != null) webkitgtk;

  installPhase = ''
    # Unpack tarball.
    mkdir -p $out
    tar xfz $src -C $out

    # Create desktop item.
    mkdir -p $out/share/applications
    cp ${desktopItem}/share/applications/* $out/share/applications
    mkdir -p $out/share/pixmaps
    ln -s $out/${name}/icon.xpm $out/share/pixmaps/${exec}.xpm

    # Patch binaries.
    interpreter=$(echo ${stdenv.glibc.out}/lib/ld-linux*.so.2)
    libCairo=$out/${name}/libcairo-swt.so
    patchelf --set-interpreter $interpreter $out/${name}/${exec}
    [ -f $libCairo ] && patchelf --set-rpath ${
      stdenv.lib.makeLibraryPath [ freetype fontconfig libX11 libXrender zlib ]
    } $libCairo

    makeWrapper $out/${name}/${exec} $out/bin/${exec} \
      --prefix PATH : ${jdk}/bin \
      --prefix LD_LIBRARY_PATH : ${
        stdenv.lib.makeLibraryPath ([ glib gtk libXtst ]
          ++ stdenv.lib.optional (webkitgtk != null) webkitgtk)
      } \
  '';

  meta = {
    inherit description;

    homepage = "https://www.eclipse.org/papyrus-rt/";
    platforms = [ "x86_64-linux" ];
  };
}
