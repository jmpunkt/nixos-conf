{
  lib,
  stdenv,
  python37,
  fetchFromGitHub,
  cmake,
  pkgconfig,
  doxygen,
  python37Packages,
  freetype,
  SDL2,
  SDL2_image,
  opusfile,
  epoxy,
  harfbuzz,
  eigen,
  qt5,
  vulkan-headers,
  vulkan-loader,
  libogg,
  jmpunkt,
}:
stdenv.mkDerivation
rec {
  pname = "openage";
  version = "2020-07-13";
  src =
    fetchFromGitHub
    {
      owner = "SFTtech";
      repo = "openage";
      rev = "3b2d966e69af35b1b3e6d47f4ebcb06f63a7fba0";
      sha256 = "1mha92aq84a917ji8jg3qdf1gbpfwx47bngd29ng7b10yd9n6wx5";
    };
  nativeBuildInputs = [cmake pkgconfig doxygen vulkan-headers python37Packages.cython python37.pkgs.wrapPython];
  buildInputs = [
    python37Packages.pillow
    python37Packages.pygments
    python37Packages.jinja2
    python37Packages.numpy
    freetype
    SDL2
    SDL2_image
    epoxy
    harfbuzz
    eigen
    opusfile
    libogg
    vulkan-loader
    qt5.qtbase
    qt5.qtquickcontrols
    jmpunkt.nyan-project
  ];
  outputs = ["out" "lib"];
  cmakeFlags = [
    "-DCMAKE_PY_INSTALL_PREFIX=${placeholder "out"}/${python37.sitePackages}"
    "-DGLOBAL_CONFIG_DIR=${placeholder "out"}/etc/openage"
  ];
  postInstall = ''
    wrapProgram "$out/bin/openage" --set PYTHONPATH "$PYTHONPATH:$out/${python37.sitePackages}"
  '';
  meta = with lib; {
    description = "Free (as in freedom) open source clone of the Age of Empires II engine";
    homepage = "https://github.com/SFTtech/openage/";
    license = licenses.lgpl3Plus;
    platforms = with platforms; unix;
    maintainers = with maintainers; [];
  };
}
