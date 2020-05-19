with import <nixpkgs> { };

let
  fhs = pkgs.buildFHSUserEnv {
    name = "anki-env";
    targetPkgs = pkgs:
      with pkgs; [
        coreutils
        python3
        glibc
        glib
        libGL
        openssl
        qt5.qtbase
        nss
        nspr
        freetype
        expat
        fontconfig
        xorg.libX11
        xorg.libXcomposite
        xorg.libXcursor
        xorg.libXdamage
        xorg.libXfixes
        xorg.libXi
        xorg.libXrender
        xorg.libXrandr
        xorg.libxcb
        systemd
        libxkbcommon
        zlib
      ];
    runScript = "bash";
  };
in pkgs.stdenv.mkDerivation {
  name = "anki-env-shell";
  nativeBuildInputs = [ fhs ];
}
